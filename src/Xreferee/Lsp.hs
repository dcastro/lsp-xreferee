module Xreferee.Lsp where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Concurrent
import Control.Exception qualified as E
import Control.Lens hiding (Iso)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.Int (Int32)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as SM
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
import Language.LSP.Protocol.Types (UInt, Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Language.LSP.VFS qualified as VFS
import Prettyprinter
import System.Directory qualified as Dir
import System.Exit
import System.IO
import Text.Pretty.Simple (pShowNoColor)
import Unsafe.Coerce qualified as Unsafe
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Types (LabelLoc (..), Symbols (..))
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
  workspaceDir <- Dir.getCurrentDirectory
  let symbols = Types.mkSymbols workspaceDir searchResult
  logger <& ("Symbols: " <> (T.pack $ show symbols)) `WithSeverity` Debug
  pure
    AppState
      { symbols,
        filesWithDiagnostics = Set.empty,
        fileVersions = SM.empty
      }

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
      notificationHandler LSP.SMethod_TextDocumentDidOpen (handleDidOpen logger),
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        logger L.<& ("Configuration changed: " <> T.pack (show (msg, cfg))) `WithSeverity` Info
        sendNotification LSP.SMethod_WindowShowMessage $
          LSP.ShowMessageParams LSP.MessageType_Info $
            "Wibble factor set to " <> T.pack (show cfg.wibbleFactor),
      notificationHandler LSP.SMethod_TextDocumentDidChange (handleDidChange logger),
      requestHandler LSP.SMethod_TextDocumentPrepareRename (handlePrepareRename logger),
      requestHandler LSP.SMethod_TextDocumentRename (handleRename logger),
      requestHandler LSP.SMethod_TextDocumentDefinition (handleDefinition logger)
    ]

handleDefinition :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDefinition
handleDefinition logger = \req responder -> do
  logReq logger req

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri
  let reqPos = req ^. LSP.params ^. LSP.position

  state <- getState

  case findSymbolAtPosition reqUri reqPos state.symbols.references of
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
            let refRange =
                  LSP.Range
                    (LSP.Position refLoc.lineNum refLoc.columnRange.start)
                    (LSP.Position refLoc.lineNum (refLoc.columnRange.end + 1))
            let anchorRange = labelLocToLspRange anchorLoc
            pure $
              LSP.DefinitionLink
                LSP.LocationLink
                  { _originSelectionRange = Just (refRange),
                    _targetUri = anchorLoc.uri,
                    _targetRange = anchorRange,
                    _targetSelectionRange = anchorRange
                  }
          responder $ Right $ LSP.InR (LSP.InL locs)

-- | Handle `didOpen` notifications.
--
-- When a file is opened, it may not necessarily reflect the state of the file on disk.
-- There are at least 2 situations where this can happen:
-- 1. When a user has unsaved changes in the file and restarts the editor,
--    the LSP will receive a `didOpen` with the contents of the "dirty" in-memory buffer.
-- 2. When the user starts the editor and quickly starts typing into the in-memory buffer before
--    the LSP server has been loaded.
--
-- This handler checks if the in-memory buffer is in a "dirty" state,
-- and if so, it reparses the file and updates the symbols.
handleDidOpen :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDidOpen
handleDidOpen logger = \req -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let fileVersion = req ^. LSP.params . LSP.textDocument . LSP.version
  let contents = req ^. LSP.params . LSP.textDocument . LSP.text

  -- Parse the symbols for this file
  let newSymbols =
        (T.lines contents `zip` [0 ..])
          <&> ( \(line, lineNum) ->
                  let (anchors, refs) = X.parseLabels (LT.fromStrict line) 1 -- 1-based columns
                      mkLoc columnRange =
                        LabelLoc
                          { uri,
                            lineNum,
                            columnRange = Types.mkColumnRange columnRange
                          }
                   in Symbols
                        { anchors = Map.fromListWith (<>) [(anchor, [mkLoc range]) | (anchor, range) <- anchors],
                          references = Map.fromListWith (<>) [(ref, [mkLoc range]) | (ref, range) <- refs]
                        }
              )
          & mconcat

  modifyState \appState0 -> do
    if not (checkIsDirty uri fileVersion appState0)
      then
        pure appState0
      else do
        -- Remove the symbols for this file
        let removeLoc loc = if loc.uri == uri then Nothing else Just loc
        let removeLocs locs =
              let locs' = locs & Maybe.mapMaybe removeLoc
               in if null locs' then Nothing else Just locs'
        let appState1 =
              appState0
                { symbols =
                    appState0.symbols
                      { Types.anchors = Map.mapMaybe removeLocs appState0.symbols.anchors,
                        Types.references = Map.mapMaybe removeLocs appState0.symbols.references
                      }
                }

        -- Add the new symbols for this file
        let appState2 =
              appState1
                { symbols = appState1.symbols <> newSymbols,
                  -- Update the version we have for this file.
                  fileVersions = SM.insert uri fileVersion appState1.fileVersions
                }

        -- If the symbols didn't change, then the diagnostics won't change either, so we can skip computing diagnostics.
        if appState0.symbols == appState2.symbols
          then pure appState2
          else sendDiagnostics logger appState2
  where
    checkIsDirty :: Uri -> Int32 -> AppState -> Bool
    checkIsDirty uri fileVersion appState =
      let lastSeenVersion = SM.findWithDefault 1 uri appState.fileVersions
       in fileVersion /= lastSeenVersion

handleDidChange :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDidChange
handleDidChange logger = \req -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  vf <- Maybe.fromJust <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)
  let rope = vf ^. VFS.file_text

  let diffs = req ^. LSP.params . LSP.contentChanges

  modifyState \appState0 -> do
    ApplyChangesResult linesToParse appState1 <- applyChanges logger appState0 uri diffs

    let newSymbols =
          linesToParse
            <&> ( \lineNum ->
                    let line = rope & Rope.getLine (fromIntegral @UInt @Word lineNum) & Rope.toText
                        (anchors, refs) = X.parseLabels (LT.fromStrict line) 1 -- 1-based columns
                        mkLoc columnRange =
                          LabelLoc
                            { uri,
                              lineNum,
                              columnRange = Types.mkColumnRange columnRange
                            }
                     in Symbols
                          { anchors = Map.fromListWith (<>) [(anchor, [mkLoc range]) | (anchor, range) <- anchors],
                            references = Map.fromListWith (<>) [(ref, [mkLoc range]) | (ref, range) <- refs]
                          }
                )
            & mconcat
        appState2 =
          appState1
            { symbols = appState1.symbols <> newSymbols,
              -- Update the version we have for this file.
              fileVersions = SM.insert uri (vf ^. VFS.lsp_version) appState1.fileVersions
            }

    -- If the symbols didn't change, then the diagnostics won't change either, so we can skip computing diagnostics.
    if appState0.symbols == appState2.symbols
      then pure appState2
      else sendDiagnostics logger appState2

-- | Calculates which lines we'll need to reparse after applying the given diffs.
-- Removes anchors/refs that are on lines that were modified by the diffs,
-- and updates the line numbers of anchors/refs that are located after the diffs.
applyChanges :: AppLogger -> AppState -> Uri -> [LSP.TextDocumentContentChangeEvent] -> AppM ApplyChangesResult
applyChanges _logger appState uri diffs =
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
              newLineCount = fromIntegral @Int @UInt $ diff ^. LSP.text . to (T.count "\n") + 1

              -- How many lines were added (or removed) by this diff.
              lineDelta = newLineCount - oldLineCount

              updateLoc :: LabelLoc -> Maybe LabelLoc
              updateLoc loc =
                if loc.uri /= uri
                  then
                    -- Anchors/refs from other files are unaffected
                    Just loc
                  else
                    -- Discard anchors/refs on lines that were modified
                    if loc.lineNum >= oldLineStart && loc.lineNum <= oldLineEnd
                      then do
                        Nothing
                      else
                        -- Update the line numbers of anchors/refs that are after the diff
                        if loc.lineNum > oldLineEnd
                          then do
                            Just loc {lineNum = loc.lineNum + lineDelta}
                          else
                            -- Anchors/refs from lines before the diff are unaffected
                            Just loc

              -- Update the line numbers we need to reparse.
              -- If they occur after this diff, they need to be shifted by the line delta, just like the anchors/refs.
              linesToParse0 = result.linesToParse <&> (\lineNum -> if lineNum > oldLineEnd then lineNum + lineDelta else lineNum)

              -- We'll need to reparse all the lines that were modified by this diff.
              -- NOTE: we don't parse them _straight_ away, because the VFS only has the state of the file after all the diffs have been applied,
              -- so we need to wait until the end of the function to parse them, once we've processed all the diffs and updated our state accordingly.
              linesToParse1 = linesToParse0 <> [oldLineStart .. oldLineStart + newLineCount - 1]

          let anchors0 =
                result.appState.symbols.anchors
                  & Map.mapMaybe
                    ( \locs ->
                        let locs' = locs & Maybe.mapMaybe updateLoc
                         in if null locs' then Nothing else Just locs'
                    )

          let refs0 =
                result.appState.symbols.references
                  & Map.mapMaybe
                    ( \locs ->
                        let locs' = locs & Maybe.mapMaybe updateLoc
                         in if null locs' then Nothing else Just locs'
                    )

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
  { linesToParse :: [UInt],
    appState :: AppState
  }

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareRename
handlePrepareRename :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentPrepareRename
handlePrepareRename logger = \req responder -> do
  logReq logger req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let pos = req ^. LSP.params . LSP.position

  state <- getState
  -- NOTE: looking up in the symbols table may not be the best solution at the moment (re-parsing the line may be faster).
  -- But after we move the symbols table to an `IxSet`, then a symbol lookup may indeed be better.
  let maybeMatch = case (findSymbolAtPosition uri pos state.symbols.anchors, findSymbolAtPosition uri pos state.symbols.references) of
        (Just (anchor, anchorLoc), _) -> Just (X.toLabel anchor, anchorLoc)
        (_, Just (ref, refLoc)) -> Just (X.toLabel ref, refLoc)
        (Nothing, Nothing) -> Nothing

  case maybeMatch of
    Nothing -> responder $ Right $ LSP.InR LSP.Null
    Just (label, loc) ->
      responder $
        Right $
          LSP.InL $
            LSP.PrepareRenameResult $
              LSP.InR $
                LSP.InL $
                  LSP.PrepareRenamePlaceholder
                    { _range = labelLocToLspRange loc,
                      _placeholder = label
                    }

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
handleRename :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentRename
handleRename logger = \req responder -> do
  logReq logger req
  pure ()

----------------------------------------------------------------------------
-- Diagnostics
----------------------------------------------------------------------------

-- | A label that is shown next to each warning/error.
diagnosticsSource :: Maybe Text
diagnosticsSource = Just "xreferee"

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: AppLogger -> AppState -> AppM AppState
sendDiagnostics _logger state = do
  -- Map keys have role "nominal", so we can't coerce between Anchors and References.
  -- Even though they are both newtypes of `Text`, it's not safe to coerce a map of Anchors into a map of References because
  -- they may have different implementations of `Ord` and thus be ordered differently in the map.
  -- However, we do know that `Anchor` and `Reference` have the same `Ord` implementation,
  -- so we use `unsafeCoerce`.
  let unusedAnchors = Map.difference state.symbols.anchors (Unsafe.unsafeCoerce state.symbols.references)
  let brokenRefs = Map.difference state.symbols.references (Unsafe.unsafeCoerce state.symbols.anchors)
  let duplicateAnchors = Map.filter (\locs -> length locs > 1) state.symbols.anchors

  let unusedAnchorsDiagnostics = do
        (anchor, anchorLocs) <- Map.toList unusedAnchors
        anchorLoc <- anchorLocs
        pure
          ( anchorLoc.uri,
            [ LSP.Diagnostic
                { _range = labelLocToLspRange anchorLoc,
                  _severity = Just LSP.DiagnosticSeverity_Warning,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Unused anchor: '" <> X.toLabel anchor <> "'",
                  _tags = Just ([LSP.DiagnosticTag_Unnecessary]),
                  _relatedInformation = Nothing,
                  _data_ = Nothing
                }
            ]
          )

  let brokenRefsDiagnostics = do
        (ref, refLocs) <- Map.toList brokenRefs
        refLoc <- refLocs
        pure
          ( refLoc.uri,
            [ LSP.Diagnostic
                { _range = labelLocToLspRange refLoc,
                  _severity = Just LSP.DiagnosticSeverity_Error,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Broken reference: '" <> X.toLabel ref <> "'",
                  _tags = Nothing,
                  _relatedInformation = Nothing,
                  _data_ = Nothing
                }
            ]
          )

  let duplicateAnchorsDiagnostics = do
        (anchor, anchorLocs) <- Map.toList duplicateAnchors
        guard (length anchorLocs > 1)
        anchorLoc <- anchorLocs
        let otherLocs = filter (/= anchorLoc) anchorLocs
        pure
          ( anchorLoc.uri,
            [ LSP.Diagnostic
                { _range = labelLocToLspRange anchorLoc,
                  _severity = Just LSP.DiagnosticSeverity_Error,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Duplicate anchor: '" <> X.toLabel anchor <> "'",
                  _tags = Nothing,
                  _relatedInformation =
                    Just $
                      otherLocs <&> \otherLoc ->
                        LSP.DiagnosticRelatedInformation
                          { _location = labelLocToLspLocation otherLoc,
                            _message = "Duplicate definition."
                          },
                  _data_ = Nothing
                }
            ]
          )

  -- Publish all diagnostics
  let allDiagnosticsByFile = Map.fromListWith (<>) $ unusedAnchorsDiagnostics <> brokenRefsDiagnostics <> duplicateAnchorsDiagnostics
  forM_ (Map.toList allDiagnosticsByFile) $ \(uri, diagnostics) -> do
    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diagnostics)

  -- Clear diagnostics for files that had diagnostics before but don't have any now.
  let filesWithDiagnosticsNow = Map.keysSet allDiagnosticsByFile
  let filesWithDiagnosticsBefore = state.filesWithDiagnostics
  forM_ (Set.difference filesWithDiagnosticsBefore filesWithDiagnosticsNow) \uri -> do
    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (Map.singleton diagnosticsSource mempty)

  pure state {filesWithDiagnostics = filesWithDiagnosticsNow}

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

labelLocToLspRange :: LabelLoc -> LSP.Range
labelLocToLspRange loc =
  LSP.Range
    { _start =
        LSP.Position
          { _line = loc.lineNum,
            _character = loc.columnRange.start
          },
      _end =
        LSP.Position
          { _line = loc.lineNum,
            _character = loc.columnRange.end + 1
          }
    }

labelLocToLspLocation :: LabelLoc -> LSP.Location
labelLocToLspLocation loc =
  LSP.Location
    { _uri = loc.uri,
      _range = labelLocToLspRange loc
    }

findSymbolAtPosition :: Uri -> LSP.Position -> Map symbol [LabelLoc] -> Maybe (symbol, LabelLoc)
findSymbolAtPosition reqUri reqPos symbols =
  let reqLine = reqPos ^. LSP.line
      reqColumn = reqPos ^. LSP.character
   in symbols
        & Map.toList
        & Maybe.mapMaybe
          ( \(ref, locs) -> do
              refLoc <-
                List.find
                  ( \loc ->
                      loc.uri == reqUri
                        && loc.lineNum == reqLine
                        && loc.columnRange.start <= reqColumn
                        && reqColumn <= loc.columnRange.end
                  )
                  locs
              Just (ref, refLoc)
          )
        & Maybe.listToMaybe
