module Xreferee.Lsp where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Concurrent
import Control.Exception qualified as E
import Control.Lens hiding (Iso)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Mixed.Rope qualified as Rope
import Language.LSP.Diagnostics
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Language.LSP.VFS qualified as VFS
import Prettyprinter
import System.Directory qualified as Dir
import System.Exit
import System.IO
import Text.Pretty.Simple (pShowNoColor)
import Unsafe.Coerce qualified as Unsafe
import XReferee.SearchResult (SearchResult (..))
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Types (LabelLoc (..))
import Xreferee.Lsp.Types qualified as Types

main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c

searchOpts :: X.SearchOpts
searchOpts =
  X.SearchOpts
    { ignores = []
    }

run :: IO Int
run = flip E.catches handlers $ do
  logFileHandle <- do
    -- TODO: change this to a proper path
    let logFilePath = "/home/dc/Dropbox/Projects/xreferee/lsp-xreferee/log.log"
    logFileHandle <- openFile logFilePath AppendMode
    hSetBuffering logFileHandle NoBuffering
    pure logFileHandle

  let -- Three loggers:
      -- 1. To stderr (shows up in the "Output" panel in vscode)
      -- 2. To the client (shows up as a user notification, filtered by severity)
      -- 3. To both
      stderrLogger :: LogAction IO (WithSeverity Text)
      stderrLogger = L.cmap show L.logStringStderr

      clientLogger :: (MonadLsp Config m) => LogAction m (WithSeverity Text)
      clientLogger = defaultClientLogger

      fileLogger :: LogAction IO (WithSeverity Text)
      fileLogger = LogAction $ \msg -> hPutStrLn logFileHandle (T.unpack $ getMsg msg)

      allLoggers :: (MonadLsp Config m) => LogAction m (WithSeverity Text)
      allLoggers =
        clientLogger <> L.hoistLogAction liftIO stderrLogger <> L.hoistLogAction liftIO fileLogger

      serverDefinition =
        ServerDefinition
          { defaultConfig = Config {fooTheBar = False, wibbleFactor = 0},
            parseConfig = \_old v -> do
              case J.fromJSON v of
                J.Error _e ->
                  -- TODO review config
                  -- J.Error e -> Left (T.pack e)
                  Right $ Config {fooTheBar = False, wibbleFactor = 0}
                J.Success cfg -> Right cfg,
            onConfigChange = const $ pure (),
            -- TODO: config section
            configSection = "lsp-xreferee",
            doInitialize = \env _initializeMsg -> do
              appState <- initialize fileLogger >>= newMVar
              pure (Right (env, appState)),
            staticHandlers = \_caps -> handle allLoggers,
            interpretHandler = \(env, appState) -> Iso (runAppM appState env) liftIO,
            options = lspOptions
          }

  let logToText = T.pack . show . pretty
  runServerWithHandles
    -- Log to both the client and stderr when we can, stderr beforehand
    (L.cmap (fmap logToText) (stderrLogger <> fileLogger))
    (L.cmap (fmap logToText) allLoggers)
    stdin
    stdout
    serverDefinition
  where
    handlers =
      [ E.Handler ioExcept,
        E.Handler someExcept
      ]
    ioExcept (e :: E.IOException) = print e >> return 1
    someExcept (e :: E.SomeException) = print e >> return 1

initialize :: LogAction IO (WithSeverity Text) -> IO AppState
initialize logger = do
  searchResult <- liftIO $ X.findRefsFromGit searchOpts
  let symbols = Types.mkSymbols searchResult
  logger <& ("Symbols: " <> (T.pack $ show symbols)) `WithSeverity` Debug
  pure AppState {symbols, filesWithDiagnostics = Set.empty}

-- ---------------------------------------------------------------------

-- TODO: review these
syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { -- We need to process the open and close notifications to keep the VFS up to date.
      LSP._openClose = Just True,
      LSP._change = Just LSP.TextDocumentSyncKind_Incremental,
      LSP._willSave = Just False,
      LSP._willSaveWaitUntil = Just False,
      LSP._save = Just $ LSP.InL $ False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { -- TODO: review this
      optTextDocumentSync = Just syncOptions
    }

-- ---------------------------------------------------------------------

logReq :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TRequestMessage a -> AppM ()
logReq logger msg = do
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

logNot :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TNotificationMessage a -> AppM ()
logNot logger msg = do
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

-- | Where the actual logic resides for handling requests and notifications.
handle :: AppLogger -> Handlers AppM
handle logger =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        modifyState $ sendDiagnostics logger,
      notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
        modifyState $ sendDiagnostics logger,
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        logger L.<& ("Configuration changed: " <> T.pack (show (msg, cfg))) `WithSeverity` Info
        sendNotification LSP.SMethod_WindowShowMessage $
          LSP.ShowMessageParams LSP.MessageType_Info $
            "Wibble factor set to " <> T.pack (show cfg.wibbleFactor),
      notificationHandler LSP.SMethod_TextDocumentDidChange (handleDidChange logger),
      requestHandler LSP.SMethod_TextDocumentRename $ \req responder -> do
        logger <& "Processing a textDocument/rename request" `WithSeverity` Info
        let params = req ^. LSP.params
            LSP.Position l c = params ^. LSP.position
            newName = params ^. LSP.newName
        vdoc <- getVersionedTextDoc (params ^. LSP.textDocument)
        -- Replace some text at the position with what the user entered
        let edit = LSP.InL $ LSP.TextEdit (LSP.mkRange l c l (c + fromIntegral (T.length newName))) newName
            tde = LSP.TextDocumentEdit (LSP._versionedTextDocumentIdentifier # vdoc) [edit]
            -- "documentChanges" field is preferred over "changes"
            rsp = LSP.WorkspaceEdit Nothing (Just [LSP.InL tde]) Nothing
        responder (Right $ LSP.InL rsp),
      requestHandler LSP.SMethod_TextDocumentDefinition (handleDefinition logger)
    ]

handleDefinition :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDefinition
handleDefinition logger = \req responder -> do
  logReq logger req

  -- TODO: Make paths absolute after xreferee is run, convert to Uri
  -- TODO: line should be 0-based, xrefcheck uses 1-based
  -- TODO: xreferee: make column 1-based, like `git grep`, but then convert to 0-based here for LSP

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri . to LSP.uriToFilePath & Maybe.fromJust
  let reqPos = req ^. LSP.params ^. LSP.position
  let reqLine = reqPos ^. LSP.line
  let reqColumn = reqPos ^. LSP.character

  reqPath <- liftIO $ Dir.makeRelativeToCurrentDirectory reqUri

  state <- getState
  let refMatch =
        state.symbols.references
          & Map.toList
          & Maybe.mapMaybe
            ( \(ref, locs) -> do
                refLoc <-
                  List.find
                    ( \loc ->
                        loc.filepath == reqPath
                          && xToLsp loc.lineNum == reqLine
                          && xToLsp loc.columnRange.start <= reqColumn
                          && reqColumn <= xToLsp loc.columnRange.end
                    )
                    locs
                Just (ref, refLoc)
            )
          & Maybe.listToMaybe

  case refMatch of
    Nothing ->
      responder $ Right $ LSP.InR (LSP.InR LSP.Null)
    Just (ref, refLoc) -> do
      -- Find the corresponding anchor(s)
      let anchor = ref & X.toLabel & X.fromLabel @X.Anchor
      let anchorMatch = state.symbols.anchors & Map.lookup anchor
      case anchorMatch of
        Nothing ->
          -- We found the reference, but there is no matching anchor.
          responder $ Right $ LSP.InR (LSP.InR LSP.Null)
        Just anchorLocs -> do
          locs <- forM anchorLocs \anchorLoc -> do
            -- TODO:
            anchorUri <- liftIO $ Dir.makeAbsolute anchorLoc.filepath <&> LSP.filePathToUri

            let refRange =
                  LSP.Range
                    (LSP.Position reqLine (xToLsp refLoc.columnRange.start))
                    (LSP.Position reqLine (xToLsp refLoc.columnRange.end + 1))
            let anchorRange =
                  LSP.Range
                    (LSP.Position (xToLsp anchorLoc.lineNum) (xToLsp anchorLoc.columnRange.start))
                    (LSP.Position (xToLsp anchorLoc.lineNum) (xToLsp anchorLoc.columnRange.end + 1))
            pure $
              LSP.DefinitionLink
                LSP.LocationLink
                  { _originSelectionRange = Just (refRange),
                    _targetUri = anchorUri,
                    _targetRange = anchorRange,
                    _targetSelectionRange = anchorRange
                  }
          responder $ Right $ LSP.InR (LSP.InL locs)

handleDidChange :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDidChange
handleDidChange logger = \req -> do
  logNot logger req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  filePath <- liftIO $ Dir.makeRelativeToCurrentDirectory $ uri & LSP.uriToFilePath & Maybe.fromJust
  vf <- Maybe.fromJust <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)
  let rope = vf ^. VFS.file_text

  let diffs = req ^. LSP.params . LSP.contentChanges

  modifyState \appState0 -> do
    ApplyChangesResult linesToParse appState1 <- applyChanges logger appState0 filePath diffs

    let newSymbols =
          Types.mkSymbols $
            linesToParse
              <&> ( \lineNum ->
                      let line = rope & Rope.getLine (fromIntegral @Int @Word lineNum) & Rope.toText
                          (anchors, refs) = X.parseLabels (LT.fromStrict line) 1 -- 1-based columns
                          mkLoc columnRange =
                            X.LabelLoc
                              { filepath = filePath,
                                lineNum = lspToX $ fromIntegral lineNum,
                                columnRange
                              }
                       in SearchResult
                            { anchors = Map.fromListWith (<>) [(anchor, [mkLoc range]) | (anchor, range) <- anchors],
                              references = Map.fromListWith (<>) [(ref, [mkLoc range]) | (ref, range) <- refs]
                            }
                  )
              & mconcat
        appState2 = appState1 {symbols = appState1.symbols <> newSymbols}

    -- If the symbols didn't change, then the diagnostics won't change either, so we can skip computing diagnostics.
    if appState0.symbols == appState2.symbols
      then pure appState2
      else sendDiagnostics logger appState2

-- | Calculates which lines we'll need to reparse after applying the given diffs.
-- Removes anchors/refs that are on lines that were modified by the diffs,
-- and updates the line numbers of anchors/refs that are located after the diffs.
applyChanges :: AppLogger -> AppState -> FilePath -> [LSP.TextDocumentContentChangeEvent] -> AppM ApplyChangesResult
applyChanges _logger appState filePath diffs =
  let initialState = ApplyChangesResult {linesToParse = [], appState = appState}
   in foldM go initialState diffs
  where
    go :: ApplyChangesResult -> LSP.TextDocumentContentChangeEvent -> AppM ApplyChangesResult
    go result diff =
      case diff of
        LSP.TextDocumentContentChangeEvent (LSP.InR _wholeDoc) -> error "We should only get partial document updates, not whole document updates"
        LSP.TextDocumentContentChangeEvent (LSP.InL diff) -> do
          let oldLineStart = diff ^. LSP.range . LSP.start . LSP.line
              oldLineEnd = diff ^. LSP.range . LSP.end . LSP.line
              oldLineCount = oldLineEnd - oldLineStart + 1
              newLineCount = diff ^. LSP.text . to (T.count "\n") + 1

              -- How many lines were added (or removed) by this diff.
              lineDelta = newLineCount - fromIntegral @UInt @Int oldLineCount

              updateLoc :: LabelLoc -> AppM (Maybe LabelLoc)
              updateLoc loc =
                if loc.filepath /= filePath
                  then
                    -- Anchors/refs from other files are unaffected
                    pure $ Just loc
                  else
                    -- Discard anchors/refs on lines that were modified
                    if xToLsp loc.lineNum >= oldLineStart && xToLsp loc.lineNum <= oldLineEnd
                      then do
                        pure Nothing
                      else
                        -- Update the line numbers of anchors/refs that are after the diff
                        if xToLsp loc.lineNum > oldLineEnd
                          then do
                            pure $
                              Just loc {lineNum = loc.lineNum + lineDelta}
                          else
                            -- Anchors/refs from lines before the diff are unaffected
                            pure $ Just loc

              -- Update the line numbers we need to reparse.
              -- If they occur after this diff, they need to be shifted by the line delta, just like the anchors/refs.
              linesToParse0 = result.linesToParse <&> (\lineNum -> if lineNum > fromIntegral @UInt @Int oldLineEnd then lineNum + lineDelta else lineNum)

              -- We'll need to reparse all the lines that were modified by this diff.
              -- NOTE: we don't parse them _straight_ away, because the VFS only has the state of the file after all the diffs have been applied,
              -- so we need to wait until the end of the function to parse them, once we've processed all the diffs and updated our state accordingly.
              linesToParse1 = linesToParse0 <> [fromIntegral @UInt @Int oldLineStart .. fromIntegral @UInt @Int oldLineStart + newLineCount - 1]

          anchors0 <-
            result.appState.symbols.anchors
              & Map.toList
              & traverse
                ( \(anchor, locs) -> do
                    locs' <- locs & traverse updateLoc <&> Maybe.catMaybes
                    pure $ if null locs' then Nothing else Just (anchor, locs')
                )
              <&> Maybe.catMaybes
              <&> Map.fromList

          refs0 <-
            result.appState.symbols.references
              & Map.toList
              & traverse
                ( \(ref, locs) -> do
                    locs' <- locs & traverse updateLoc <&> Maybe.catMaybes
                    pure $ if null locs' then Nothing else Just (ref, locs')
                )
              <&> Maybe.catMaybes
              <&> Map.fromList

          pure $
            result
              { linesToParse = linesToParse1,
                appState =
                  result.appState
                    { symbols =
                        result.appState.symbols
                          { Types.anchors = anchors0,
                            Types.references = refs0
                          }
                    }
              }

data ApplyChangesResult = ApplyChangesResult
  { linesToParse :: [Int],
    appState :: AppState
  }

-- | A label that is shown next to each warning/error.
diagnosticsSource :: Maybe Text
diagnosticsSource = Just "xreferee"

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: AppLogger -> AppState -> AppM AppState
sendDiagnostics logger state = do
  -- Map keys have role "nominal", so we can't coerce between Anchors and References.
  -- Even though they are both newtypes of `Text`, it's not safe to coerce a map of Anchors into a map of References because
  -- they may have different implementations of `Ord` and thus be ordered differently in the map.
  -- However, we do know that `Anchor` and `Reference` have the same `Ord` implementation,
  -- so we use `unsafeCoerce`.
  let unusedAnchors = Map.difference state.symbols.anchors (Unsafe.unsafeCoerce state.symbols.references)
  let brokenRefs = Map.difference state.symbols.references (Unsafe.unsafeCoerce state.symbols.anchors)
  let duplicateAnchors = Map.filter (\locs -> length locs > 1) state.symbols.anchors

  let unusedAnchorsDiagnostics = do
        (_, anchorLocs) <- Map.toList unusedAnchors
        anchorLoc <- anchorLocs
        pure
          ( anchorLoc.filepath,
            [ LSP.Diagnostic
                { _range = xLocToLspRange anchorLoc,
                  _severity = Just LSP.DiagnosticSeverity_Warning,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Unused anchor.",
                  _tags = Just ([LSP.DiagnosticTag_Unnecessary]),
                  _relatedInformation = Nothing,
                  _data_ = Nothing
                }
            ]
          )

  let brokenRefsDiagnostics = do
        (_, refLocs) <- Map.toList brokenRefs
        refLoc <- refLocs
        pure
          ( refLoc.filepath,
            [ LSP.Diagnostic
                { _range = xLocToLspRange refLoc,
                  _severity = Just LSP.DiagnosticSeverity_Error,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Broken reference.",
                  _tags = Nothing,
                  _relatedInformation = Nothing,
                  _data_ = Nothing
                }
            ]
          )

  -- TODO: duplicate anchors

  -- Publish all diagnostics
  let allDiagnosticsByFile = Map.fromListWith (<>) $ unusedAnchorsDiagnostics <> brokenRefsDiagnostics

  forM_ (Map.toList allDiagnosticsByFile) $ \(filePath, diagnostics) -> do
    uri <- liftIO $ Dir.makeAbsolute filePath <&> LSP.filePathToUri
    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diagnostics)

  -- Clear diagnostics for files that had diagnostics before but don't have any now.
  let filesWithDiagnosticsNow = Map.keysSet allDiagnosticsByFile
  let filesWithDiagnosticsBefore = state.filesWithDiagnostics
  forM_ (Set.difference filesWithDiagnosticsBefore filesWithDiagnosticsNow) \filePath -> do
    uri <- liftIO $ Dir.makeAbsolute filePath <&> LSP.filePathToUri
    logger <& ("Clearing diagnostics for file: " <> (T.pack $ show uri)) `WithSeverity` Info
    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (Map.singleton diagnosticsSource mempty)

  pure state {filesWithDiagnostics = filesWithDiagnosticsNow}

-- Xreferee uses 1-based lines/columns, but LSP uses 0-based lines/columns.
xToLsp :: Int -> LSP.UInt
xToLsp xLine = fromIntegral @Int @LSP.UInt (xLine - 1)

-- Xreferee uses 1-based lines/columns, but LSP uses 0-based lines/columns.
lspToX :: LSP.UInt -> Int
lspToX xLine = fromIntegral @LSP.UInt @Int (xLine + 1)

xLocToLspRange :: LabelLoc -> LSP.Range
xLocToLspRange loc =
  LSP.Range
    { _start =
        LSP.Position
          { _line = xToLsp loc.lineNum,
            _character = xToLsp loc.columnRange.start
          },
      _end =
        LSP.Position
          { _line = xToLsp loc.lineNum,
            _character = xToLsp loc.columnRange.end
          }
    }

xLocToLspLocation :: (MonadIO m) => LabelLoc -> m LSP.Location
xLocToLspLocation loc = do
  uri <- liftIO $ Dir.makeAbsolute loc.filepath <&> LSP.filePathToUri

  pure $
    LSP.Location
      { _uri = uri,
        _range = xLocToLspRange loc
      }
