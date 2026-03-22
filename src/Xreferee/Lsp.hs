module Xreferee.Lsp where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Concurrent
import Control.Exception qualified as E
import Control.Lens hiding (Indexable, Iso)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson qualified as J
import Data.Int (Int32)
import Data.IxSet.Typed ((@<=), (@=), (@>), (@>=), (@>=<=))
import Data.IxSet.Typed qualified as Ix
import Data.IxSet.Typed.Util qualified as Ix
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as SM
import Data.Maybe qualified as Maybe
import Data.Set (Set)
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
import XReferee.SearchResult (Anchor, Reference)
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.AppM qualified as App
import Xreferee.Lsp.Types (ColumnEnd (..), ColumnStart (..), LineNum (..), SymbolEntry (..), SymbolIxsConstraint, SymbolLoc (..), SymbolSet, Symbols (..))
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

logReq' :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TRequestMessage a -> AppM ()
logReq' logger msg = do
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logNot' :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TNotificationMessage a -> AppM ()
logNot' logger msg = do
  logger <& (T.pack $ show msg) `WithSeverity` Debug

-- | Where the actual logic resides for handling requests and notifications.
handle :: AppLogger -> Handlers AppM
handle logger =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        registerDidChangeWatchedFiles logger
        modifyState $ sendDiagnostics logger,
      notificationHandler LSP.SMethod_TextDocumentDidOpen (handleDidOpen logger),
      notificationHandler LSP.SMethod_TextDocumentDidClose \_req -> do
        -- Empty handler so we don't get these warnings in the log: `LSP: no handler for: "textDocument/didClose"`
        pure (),
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        logger L.<& ("Configuration changed: " <> T.pack (show (msg, cfg))) `WithSeverity` Info
        sendNotification LSP.SMethod_WindowShowMessage $
          LSP.ShowMessageParams LSP.MessageType_Info $
            "Wibble factor set to " <> T.pack (show cfg.wibbleFactor),
      notificationHandler LSP.SMethod_TextDocumentDidChange (handleDidChange logger),
      requestHandler LSP.SMethod_TextDocumentPrepareRename (handlePrepareRename logger),
      requestHandler LSP.SMethod_TextDocumentRename (handleRename logger),
      requestHandler LSP.SMethod_TextDocumentDefinition (handleDefinition logger),
      requestHandler LSP.SMethod_TextDocumentReferences (handleReferences logger)
      -- Workspace events
      -- NOTE: `workspace/didChangeWatchedFiles` must be registered dynamically, see `registerDidChangeWatchedFiles`
    ]

-- | Ask the client to start watching files and sending `workspace/didChangeWatchedFiles` notifications.
registerDidChangeWatchedFiles :: AppLogger -> AppM ()
registerDidChangeWatchedFiles logger = do
  let watcher =
        LSP.FileSystemWatcher
          { _globPattern = LSP.GlobPattern $ LSP.InL $ LSP.Pattern "**/*",
            _kind = Nothing
          }
      registrationOptions =
        LSP.DidChangeWatchedFilesRegistrationOptions
          { _watchers = [watcher]
          }

  let coreLogger = L.cmap (fmap (T.pack . show . pretty)) logger
  result <- LSP.registerCapability coreLogger LSP.SMethod_WorkspaceDidChangeWatchedFiles registrationOptions (handleDidChangeWatchedFiles logger)

  case result of
    Nothing ->
      logger <& "Failed to register workspace/didChangeWatchedFiles watcher." `WithSeverity` Warning
    Just _token ->
      logger <& "Registered workspace/didChangeWatchedFiles watcher." `WithSeverity` Info

handleDefinition :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDefinition
handleDefinition logger = \req responder -> do
  logReq' logger req

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri
  let reqPos = req ^. LSP.params ^. LSP.position

  state <- getState

  case findSymbolAtPosition reqUri reqPos state.symbols.references of
    Nothing ->
      responder $ Right $ LSP.InR (LSP.InR LSP.Null)
    Just refEntry -> do
      -- Find the corresponding anchor(s).
      -- Ideally there will be 1, but there can also be 0 (if the reference is broken) or more than 1 (if there are duplicate anchors).
      let anchor = refEntry.symbol & X.toLabel & X.fromLabel @X.Anchor
      -- Build links from the reference to the anchor(s)
      let links =
            state.symbols.anchors
              @= anchor
              & Ix.toList
              <&> \anchorEntry ->
                let refRange =
                      LSP.Range
                        { _start = LSP.Position refEntry.loc.lineNum refEntry.loc.columnRange.start,
                          _end = LSP.Position refEntry.loc.lineNum (refEntry.loc.columnRange.end + 1)
                        }
                    anchorRange = symbolLocToLspRange anchorEntry.loc
                 in LSP.DefinitionLink
                      LSP.LocationLink
                        { _originSelectionRange = Just refRange,
                          _targetUri = anchorEntry.loc.uri,
                          _targetRange = anchorRange,
                          _targetSelectionRange = anchorRange
                        }
      responder $ Right $ LSP.InR (LSP.InL links)

handleReferences :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentReferences
handleReferences logger = \req responder -> do
  logReq' logger req

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri
  let reqPos = req ^. LSP.params ^. LSP.position

  state <- getState

  case findSymbolAtPosition reqUri reqPos state.symbols.anchors of
    Nothing ->
      responder $ Right $ LSP.InR LSP.Null
    Just anchorEntry -> do
      -- Find the corresponding references
      let ref = anchorEntry.symbol & X.toLabel & X.fromLabel @X.Reference
      let locs = state.symbols.references @= ref & Ix.toList <&> \refEntry -> symbolLocToLspLocation refEntry.loc
      responder $ Right $ LSP.InL locs

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
  logNot' logger req
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let fileVersion = req ^. LSP.params . LSP.textDocument . LSP.version
  let contents = req ^. LSP.params . LSP.textDocument . LSP.text

  -- Parse the symbols for this file
  let newSymbols =
        (T.lines contents `zip` [0 ..])
          <&> ( \(line, lineNum) ->
                  let (anchors, refs) = X.parseLabels (LT.fromStrict line) 1 -- 1-based columns
                      mkSymbolEntry :: forall symbol. symbol -> X.ColumnRange -> SymbolEntry symbol
                      mkSymbolEntry sym columnRange =
                        SymbolEntry
                          { symbol = sym,
                            loc =
                              SymbolLoc
                                { uri,
                                  lineNum,
                                  columnRange = Types.mkColumnRange columnRange
                                }
                          }
                   in Symbols
                        { anchors = anchors <&> uncurry mkSymbolEntry & Ix.fromList,
                          references = refs <&> uncurry mkSymbolEntry & Ix.fromList
                        }
              )
          & mconcat

  modifyState \appState0 -> do
    if not (checkIsDirty uri fileVersion appState0)
      then
        pure appState0
      else do
        -- Remove the symbols for this file
        let appState1 =
              appState0
                { App.symbols =
                    appState0.symbols
                      { anchors = Ix.deleteMany appState0.symbols.anchors (\anchors -> anchors @= uri),
                        references = Ix.deleteMany appState0.symbols.references (\references -> references @= uri)
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
       in -- NOTE: versions are not strictly monotonic.
          -- If a file is changed on disk (e.g. with `echo "#(ref:test4)" >> file.md`), AND the file is not currently opened in vscode,
          -- the next time the user opens it, the version will be reset to 1.
          fileVersion /= lastSeenVersion

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
handleDidChangeWatchedFiles :: AppLogger -> Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles
handleDidChangeWatchedFiles logger = \req -> do
  logNot' logger req
  pure ()

handleDidChange :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDidChange
handleDidChange logger = \req -> do
  logNot' logger req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  vf <- Maybe.fromJust <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)
  let rope = vf ^. VFS.file_text

  let diffs = req ^. LSP.params . LSP.contentChanges

  modifyState \appState0 -> do
    -- Apply the diffs to our symbols tables
    ApplyChangesResult linesToParse symbols1 <- applyChanges logger appState0.symbols uri diffs

    -- Parse any new symbols introduces by the diffs
    let newSymbols =
          linesToParse
            <&> ( \lineNum ->
                    let line = rope & Rope.getLine (fromIntegral @UInt @Word lineNum) & Rope.toText
                        (anchors, refs) = X.parseLabels (LT.fromStrict line) 1 -- 1-based columns
                        mkSymbolEntry :: forall symbol. symbol -> X.ColumnRange -> SymbolEntry symbol
                        mkSymbolEntry sym columnRange =
                          SymbolEntry
                            { symbol = sym,
                              loc =
                                SymbolLoc
                                  { uri,
                                    lineNum,
                                    columnRange = Types.mkColumnRange columnRange
                                  }
                            }
                     in Symbols
                          { anchors = anchors <&> uncurry mkSymbolEntry & Ix.fromList,
                            references = refs <&> uncurry mkSymbolEntry & Ix.fromList
                          }
                )
            & mconcat
        appState1 =
          appState0
            { symbols = symbols1 <> newSymbols,
              -- Update the version we have for this file.
              fileVersions = SM.insert uri (vf ^. VFS.lsp_version) appState0.fileVersions
            }

    -- If the symbols didn't change, then the diagnostics won't change either, so we can skip computing diagnostics.
    if appState0.symbols == appState1.symbols
      then pure appState1
      else sendDiagnostics logger appState1

-- | Calculates which lines we'll need to reparse after applying the given diffs.
-- Removes anchors/refs that are on lines that were modified by the diffs,
-- and updates the line numbers of anchors/refs that are located after the diffs.
applyChanges :: AppLogger -> Symbols -> Uri -> [LSP.TextDocumentContentChangeEvent] -> AppM ApplyChangesResult
applyChanges _logger symbols uri diffs =
  let initialState = ApplyChangesResult {linesToParse = [], symbols = symbols}
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

              -- Get the symbols defined in this file
              thisFileAnchors = result.symbols.anchors @= uri
              thisFileReferences = result.symbols.references @= uri

              -- Discard anchors/refs on lines that were modified by this diff
              deleteEntriesInDiff :: forall symbol. (SymbolIxsConstraint symbol) => SymbolSet symbol -> SymbolSet symbol -> SymbolSet symbol
              deleteEntriesInDiff thisFileSymbols allSymbols =
                let entriesToDelete = thisFileSymbols @>=<= (LineNum oldLineStart, LineNum oldLineEnd)
                 in Ix.difference allSymbols entriesToDelete

              -- Update the line numbers of anchors/refs that are after the diff
              shiftEntriesAfterDiff :: forall symbol. (SymbolIxsConstraint symbol) => SymbolSet symbol -> SymbolSet symbol -> SymbolSet symbol
              shiftEntriesAfterDiff thisFileSymbols allSymbols =
                let entriesToShift = thisFileSymbols @> LineNum oldLineEnd
                    shiftedEntries =
                      entriesToShift
                        & Ix.toList
                        <&> (\entry -> entry {loc = entry.loc {lineNum = entry.loc.lineNum + lineDelta}})
                        & Ix.fromList
                 in allSymbols
                      & Ix.difference entriesToShift
                      & Ix.union shiftedEntries

              -- Update the line numbers we need to reparse.
              -- If they occur after this diff, they need to be shifted by the line delta, just like the anchors/refs.
              linesToParse0 = result.linesToParse <&> (\lineNum -> if lineNum > oldLineEnd then lineNum + lineDelta else lineNum)

              -- We'll need to reparse all the lines that were modified by this diff.
              -- NOTE: we don't parse them _straight_ away, because the VFS only has the state of the file after all the diffs have been applied,
              -- so we need to wait until the end of the function to parse them, once we've processed all the diffs and updated our state accordingly.
              linesToParse1 = linesToParse0 <> [oldLineStart .. oldLineStart + newLineCount - 1]

          let anchors0 =
                result.symbols.anchors
                  & deleteEntriesInDiff thisFileAnchors
                  & shiftEntriesAfterDiff thisFileAnchors
          let refs0 =
                result.symbols.references
                  & deleteEntriesInDiff thisFileReferences
                  & shiftEntriesAfterDiff thisFileReferences

          pure $
            result
              { linesToParse = linesToParse1,
                symbols =
                  result.symbols
                    { anchors = anchors0,
                      references = refs0
                    }
              }

data ApplyChangesResult = ApplyChangesResult
  { linesToParse :: [UInt],
    symbols :: Symbols
  }

----------------------------------------------------------------------------
-- Renames
----------------------------------------------------------------------------

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareRename
handlePrepareRename :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentPrepareRename
handlePrepareRename _logger = \req responder -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let pos = req ^. LSP.params . LSP.position

  state <- getState
  -- NOTE: looking up in the symbols table may not be the best solution at the moment (re-parsing the line may be faster).
  -- But after we move the symbols table to an `IxSet`, then a symbol lookup may indeed be better.
  let maybeMatch = case (findSymbolAtPosition uri pos state.symbols.anchors, findSymbolAtPosition uri pos state.symbols.references) of
        (Just anchorEntry, _) -> Just (X.toLabel anchorEntry.symbol, anchorEntry.loc)
        (_, Just refEntry) -> Just (X.toLabel refEntry.symbol, refEntry.loc)
        (Nothing, Nothing) -> Nothing

  case maybeMatch of
    Nothing -> responder $ Right $ LSP.InR LSP.Null
    Just (symbol, loc) ->
      responder $
        Right $
          LSP.InL $
            LSP.PrepareRenameResult $
              LSP.InR $
                LSP.InL $
                  LSP.PrepareRenamePlaceholder
                    { _range = symbolLocToLspRange loc,
                      _placeholder = symbol
                    }

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
handleRename :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentRename
handleRename logger = \req responder -> do
  logReq' logger req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let pos = req ^. LSP.params . LSP.position
  let newLabelName = req ^. LSP.params . LSP.newName

  state <- getState

  let maybeMatch = case (findSymbolAtPosition uri pos state.symbols.anchors, findSymbolAtPosition uri pos state.symbols.references) of
        (Just anchorEntry, _) -> Just $ X.toLabel anchorEntry.symbol
        (_, Just refEntry) -> Just $ X.toLabel refEntry.symbol
        (Nothing, Nothing) -> Nothing

  case maybeMatch of
    Nothing -> responder $ Right $ LSP.InR LSP.Null
    Just label -> do
      let anchor = label & X.fromLabel @X.Anchor
      let ref = label & X.fromLabel @X.Reference

      let anchorEdits :: Map Uri [LSP.TextEdit] =
            state.symbols.anchors
              @= anchor
              & Ix.groupBy' @Uri
              <&> \entries ->
                Set.toList entries
                  <&> \entry ->
                    LSP.TextEdit
                      { _range = symbolLocToLspRange entry.loc,
                        _newText = newLabelName & X.fromLabel @X.Anchor & X.renderLabel
                      }

      let refEdits :: Map Uri [LSP.TextEdit] =
            state.symbols.references
              @= ref
              & Ix.groupBy' @Uri
              <&> \entries ->
                Set.toList entries
                  <&> \entry ->
                    LSP.TextEdit
                      { _range = symbolLocToLspRange entry.loc,
                        _newText = newLabelName & X.fromLabel @X.Reference & X.renderLabel
                      }

      let workspaceEdit =
            LSP.WorkspaceEdit
              { _changes = Just $ Map.unionWith (<>) anchorEdits refEdits,
                _documentChanges = Nothing,
                _changeAnnotations = Nothing
              }

      responder (Right $ LSP.InL workspaceEdit)

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
  let anchorsGrouped = state.symbols.anchors & Ix.groupBy' @Anchor :: Map Anchor (Set (SymbolEntry Anchor))
  let refsGrouped = state.symbols.references & Ix.groupBy' @Reference :: Map Reference (Set (SymbolEntry Reference))

  -- Map keys have role "nominal", so we can't coerce between Anchors and References.
  -- Even though they are both newtypes of `Text`, it's not safe to coerce a map of Anchors into a map of References because
  -- they may have different implementations of `Ord` and thus be ordered differently in the map.
  -- However, we do know that `Anchor` and `Reference` have the same `Ord` implementation,
  -- so we use `unsafeCoerce`.
  let unusedAnchors = Map.difference anchorsGrouped (Unsafe.unsafeCoerce refsGrouped)
  let brokenRefs = Map.difference refsGrouped (Unsafe.unsafeCoerce anchorsGrouped)
  let duplicateAnchors = Map.filter (\locs -> length locs > 1) anchorsGrouped

  let unusedAnchorsDiagnostics = do
        (anchor, entries) <- Map.toList unusedAnchors
        entry <- Set.toList entries
        pure
          ( entry.loc.uri,
            [ LSP.Diagnostic
                { _range = symbolLocToLspRange entry.loc,
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
        (ref, entries) <- Map.toList brokenRefs
        entry <- Set.toList entries
        pure
          ( entry.loc.uri,
            [ LSP.Diagnostic
                { _range = symbolLocToLspRange entry.loc,
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
        (anchor, entries) <- Map.toList duplicateAnchors
        guard (length entries > 1)
        entry <- Set.toList entries
        let otherEntries = Set.filter (/= entry) entries
        pure
          ( entry.loc.uri,
            [ LSP.Diagnostic
                { _range = symbolLocToLspRange entry.loc,
                  _severity = Just LSP.DiagnosticSeverity_Error,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Duplicate anchor: '" <> X.toLabel anchor <> "'",
                  _tags = Nothing,
                  _relatedInformation =
                    Just $
                      Set.toList otherEntries <&> \otherEntry ->
                        LSP.DiagnosticRelatedInformation
                          { _location = symbolLocToLspLocation otherEntry.loc,
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

symbolLocToLspRange :: SymbolLoc -> LSP.Range
symbolLocToLspRange loc =
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

symbolLocToLspLocation :: SymbolLoc -> LSP.Location
symbolLocToLspLocation loc =
  LSP.Location
    { _uri = loc.uri,
      _range = symbolLocToLspRange loc
    }

findSymbolAtPosition :: (SymbolIxsConstraint symbol) => Uri -> LSP.Position -> SymbolSet symbol -> Maybe (SymbolEntry symbol)
findSymbolAtPosition reqUri reqPos symbols =
  let reqLine = reqPos ^. LSP.line
      reqColumn = reqPos ^. LSP.character
   in Ix.getOne $
        symbols
          @= reqUri
          @= LineNum reqLine
          @<= ColumnStart reqColumn
          @>= ColumnEnd reqColumn
