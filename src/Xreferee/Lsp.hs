module Xreferee.Lsp where

import ClassyPrelude hiding (Handler)
import Colog.Core (LogAction (..), WithSeverity (..))
import Colog.Core qualified as L
import Control.Exception qualified as E
import Control.Lens hiding (Indexable, Iso)
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Data.Set qualified as Set
import Data.Text.IO qualified as T
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Prettyprinter
import System.Directory qualified as Dir
import System.Exit
import System.FilePath qualified as FP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Handlers.Definition (handleDefinition)
import Xreferee.Lsp.Handlers.DidChange (handleDidChange)
import Xreferee.Lsp.Handlers.DidChangeWatchedFiles (handleDidChangeWatchedFiles)
import Xreferee.Lsp.Handlers.DidOpen (handleDidOpen)
import Xreferee.Lsp.Handlers.PrepareRename (handlePrepareRename)
import Xreferee.Lsp.Handlers.References (handleReferences)
import Xreferee.Lsp.Handlers.Rename (handleRename)
import Xreferee.Lsp.Log (debug, debugP)
import Xreferee.Lsp.SendDiagnostics (modifyState, sendDiagnostics)
import Xreferee.Lsp.Types qualified as Types
import Xreferee.Lsp.Util qualified as Util

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
      fileLogger = LogAction $ \msg -> T.hPutStrLn logFileHandle (getMsg msg)

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
              appEnv <- initialize fileLogger allLoggers
              pure (Right (env, appEnv)),
            staticHandlers = \_caps -> mkHandlers,
            interpretHandler = \(env, appEnv) -> Iso (runAppM appEnv env) liftIO,
            options = lspOptions
          }

  let logToText = tshow . pretty
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

initialize :: LogAction IO (WithSeverity Text) -> AppLogger -> IO AppEnv
initialize _initLogger appLogger = do
  searchResult <- liftIO $ X.findRefsFromGit searchOpts
  workspaceDir <- Dir.getCurrentDirectory
  let symbols = Types.mkSymbols workspaceDir searchResult

  state <-
    newMVar
      AppState
        { symbols,
          filesWithDiagnostics = Set.empty,
          fileVersions = SM.empty
        }
  pure
    AppEnv
      { logger = appLogger,
        state = state,
        workspaceDir = FP.splitDirectories workspaceDir
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

-- | Where the actual logic resides for handling requests and notifications.
mkHandlers :: Handlers AppM
mkHandlers =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        registerDidChangeWatchedFiles
        modifyState $ sendDiagnostics,
      notificationHandler LSP.SMethod_TextDocumentDidOpen (filterNot handleDidOpen),
      notificationHandler LSP.SMethod_TextDocumentDidClose \_req -> do
        -- Empty handler so we don't get these warnings in the log: `LSP: no handler for: "textDocument/didClose"`
        pure (),
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        debugP "Configuration changed" (msg, cfg)
        sendNotification LSP.SMethod_WindowShowMessage $
          LSP.ShowMessageParams LSP.MessageType_Info $
            "Wibble factor set to " <> tshow cfg.wibbleFactor,
      notificationHandler LSP.SMethod_TextDocumentDidChange (filterNot handleDidChange),
      requestHandler LSP.SMethod_TextDocumentPrepareRename (filterReq handlePrepareRename),
      requestHandler LSP.SMethod_TextDocumentRename (filterReq handleRename),
      requestHandler LSP.SMethod_TextDocumentDefinition (filterReq handleDefinition),
      requestHandler LSP.SMethod_TextDocumentReferences (filterReq handleReferences)
      -- Workspace events
      -- NOTE: `workspace/didChangeWatchedFiles` must be registered dynamically, see `registerDidChangeWatchedFiles`
    ]
  where
    -- Skip the handler if we're not interested in processing events for this file
    filterReq ::
      forall from (method :: LSP.Method from 'LSP.Request) doc.
      (LSP.HasTextDocument (LSP.MessageParams method) doc) =>
      (LSP.HasUri doc LSP.Uri) =>
      Handler AppM method ->
      Handler AppM method
    filterReq handler = \msg responder -> do
      let uri = msg ^. LSP.params . LSP.textDocument . LSP.uri
      whenM (Util.shouldHandleFile uri) do
        handler msg responder

    -- Skip the handler if we're not interested in processing events for this file
    filterNot ::
      forall from (method :: LSP.Method from 'LSP.Notification) doc.
      (LSP.HasTextDocument (LSP.MessageParams method) doc) =>
      (LSP.HasUri doc LSP.Uri) =>
      Handler AppM method ->
      Handler AppM method
    filterNot handler = \msg -> do
      let uri = msg ^. LSP.params . LSP.textDocument . LSP.uri
      whenM (Util.shouldHandleFile uri) do
        handler msg

-- | Ask the client to start watching files and sending `workspace/didChangeWatchedFiles` notifications.
registerDidChangeWatchedFiles :: AppM ()
registerDidChangeWatchedFiles = do
  let watcher =
        LSP.FileSystemWatcher
          { _globPattern = LSP.GlobPattern $ LSP.InL $ LSP.Pattern "**/*",
            _kind = Nothing
          }
      registrationOptions =
        LSP.DidChangeWatchedFilesRegistrationOptions
          { _watchers = [watcher]
          }

  appLogger <- asks (.logger)
  let coreLogger = L.cmap (fmap (tshow . pretty)) appLogger
  result <- LSP.registerCapability coreLogger LSP.SMethod_WorkspaceDidChangeWatchedFiles registrationOptions handleDidChangeWatchedFiles

  case result of
    Nothing ->
      debug "Failed to register workspace/didChangeWatchedFiles watcher."
    Just _token ->
      debug "Registered workspace/didChangeWatchedFiles watcher."
