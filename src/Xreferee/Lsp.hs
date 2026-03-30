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
import Data.Version qualified as Version
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Options.Applicative qualified as Opt
import Paths_lsp_xreferee (version)
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
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Options qualified as LspOpt
import Xreferee.Lsp.SendDiagnostics (modifyState, sendDiagnostics)
import Xreferee.Lsp.Types qualified as Types
import Xreferee.Lsp.Util qualified as Util

main :: IO ()
main = do
  cliOptions <- Opt.execParser LspOpt.cliParserInfo
  if cliOptions.showVersionFlag
    then putStrLn ("v" <> pack (Version.showVersion version))
    else do
      run cliOptions >>= \case
        0 -> exitSuccess
        c -> exitWith . ExitFailure $ c

searchOpts :: X.SearchOpts
searchOpts =
  X.SearchOpts
    { ignores = []
    }

run :: LspOpt.CliOptions -> IO Int
run cliOptions = flip E.catches handlers $ do
  maybeLogFileHandle <- forM cliOptions.logFilePath \logFilePath -> do
    logFileHandle <- openFile logFilePath AppendMode
    hSetBuffering logFileHandle NoBuffering
    pure logFileHandle

  let stderrLogger :: LogAction IO (WithSeverity Text)
      stderrLogger = L.cmap show L.logStringStderr

      -- "Info" and above show up in vscode's "Output" panel.
      -- "Error" and above show up in vscode's "Output" panel + as user notifications.
      clientLogger :: (MonadLsp Config m) => LogAction m (WithSeverity Text)
      clientLogger = defaultClientLogger

      -- Log everything to a file if the user specified a log file path, otherwise do nothing.
      fileLogger :: LogAction IO (WithSeverity Text)
      fileLogger =
        maybeLogFileHandle
          <&> (\logFileHandle -> LogAction $ \msg -> T.hPutStrLn logFileHandle (getMsg msg))
          & fromMaybe mempty

      -- Log to stderr when starting up, before we have a connection to the client.
      startupLoggers :: LogAction IO (WithSeverity Text)
      startupLoggers = stderrLogger <> fileLogger

      -- After startup, log to the client and the file (if specified).
      appLoggers :: (MonadLsp Config m) => LogAction m (WithSeverity Text)
      appLoggers =
        clientLogger <> L.hoistLogAction liftIO fileLogger

      serverDefinition =
        ServerDefinition
          { defaultConfig = Config {},
            parseConfig = \_old v -> do
              case J.fromJSON v of
                J.Error _e ->
                  Right $ Config {}
                J.Success cfg -> Right cfg,
            -- TODO: config section
            onConfigChange = const $ pure (),
            configSection = "lsp-xreferee",
            doInitialize = \env _initializeMsg -> do
              appEnv <- initialize appLoggers
              pure (Right (env, appEnv)),
            staticHandlers = \_caps -> mkHandlers,
            interpretHandler = \(env, appEnv) -> Iso (runAppM appEnv env) liftIO,
            options = lspOptions
          }

  let logToText = tshow . pretty
  runServerWithHandles
    (L.cmap (fmap logToText) startupLoggers)
    (L.cmap (fmap logToText) appLoggers)
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

initialize :: AppLogger -> IO AppEnv
initialize appLogger = do
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
    { optTextDocumentSync = Just syncOptions
    }

-- ---------------------------------------------------------------------

-- | Where the actual logic resides for handling requests and notifications.
mkHandlers :: Handlers AppM
mkHandlers =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        registerDidChangeWatchedFiles
        modifyState $ sendDiagnostics
        Log.info "Server initialized",
      notificationHandler LSP.SMethod_TextDocumentDidOpen (filterNot handleDidOpen),
      notificationHandler LSP.SMethod_TextDocumentDidClose \_req -> do
        -- Empty handler so we don't get these warnings in the log: `LSP: no handler for: "textDocument/didClose"`
        pure (),
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_msg -> do
        cfg <- getConfig
        Log.debugP "Configuration changed" cfg,
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
      Log.err "Failed to register workspace/didChangeWatchedFiles watcher."
    Just _token ->
      Log.info "Registered workspace/didChangeWatchedFiles watcher."
