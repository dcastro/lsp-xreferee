module Xreferee.Lsp where

import ClassyPrelude hiding (Handler)
import Colog.Core (LogAction (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Exception qualified as E
import Control.Lens hiding (Indexable, Iso)
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Format qualified as Time
import Data.Time.LocalTime qualified as Time
import Data.Version qualified as Version
import GHC.Conc (setUncaughtExceptionHandler)
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
import System.IO qualified as SIO
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.FileWatchers qualified as FileWatchers
import Xreferee.Lsp.Git qualified as Git
import Xreferee.Lsp.Handlers.Definition (handleDefinition)
import Xreferee.Lsp.Handlers.DidChange (handleDidChange)
import Xreferee.Lsp.Handlers.DidOpen (handleDidOpen)
import Xreferee.Lsp.Handlers.PrepareRename (handlePrepareRename)
import Xreferee.Lsp.Handlers.References (handleReferences)
import Xreferee.Lsp.Handlers.Rename (handleRename)
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Options qualified as LspOpt
import Xreferee.Lsp.SendDiagnostics (sendDiagnostics)
import Xreferee.Lsp.Symbols qualified as Symbols
import Xreferee.Lsp.Util qualified as Util

main :: IO ()
main = do
  -- Catch exceptions that escape threads spawned by the `lsp` library (e.g. its
  -- request-handling threads) and would otherwise just be printed to stderr and lost,
  -- since stderr isn't visible to the user when running as an LSP server.
  -- The exception is logged to `~/.cache/lsp-xreferee/crash.log`
  setUncaughtExceptionHandler (dumpCrash "uncaught exception (background thread)")

  cliOptions <- Opt.execParser LspOpt.cliParserInfo
  if cliOptions.showVersionFlag
    then putStrLn ("v" <> pack (Version.showVersion version))
    else do
      run cliOptions >>= \case
        0 -> exitSuccess
        c -> exitWith . ExitFailure $ c

run :: LspOpt.CliOptions -> IO Int
run cliOptions = flip E.catches handlers do
  t0 <- Time.getPOSIXTime
  maybeLogFileHandle <- forM cliOptions.logFilePath \logFilePath -> do
    logFileHandle <- openFile logFilePath AppendMode
    hSetBuffering logFileHandle NoBuffering
    pure logFileHandle

  let stderrLogger :: LogAction IO (WithSeverity Text)
      stderrLogger = L.cmap show L.logStringStderr

      -- "Info" and above show up in vscode's "Output" panel.
      -- "Error" and above show up in vscode's "Output" panel + as user notifications.
      clientLogger :: (MonadLsp config m) => LogAction m (WithSeverity Text)
      clientLogger = defaultClientLogger

      -- Log everything to a file if the user specified a log file path, otherwise do nothing.
      fileLogger :: LogAction IO (WithSeverity Text)
      fileLogger =
        maybe mempty (\logFileHandle -> LogAction $ \msg -> T.hPutStrLn logFileHandle (getMsg msg)) maybeLogFileHandle

      -- During startup, before we have a connection to the client:
      --   * Log everything to stderr
      --   * Log everything to a file if the user specified a log file path
      startupLoggers :: LogAction IO (WithSeverity Text)
      startupLoggers = stderrLogger <> fileLogger

      -- After startup:
      --   * Log to the client (only Info and Error)
      --   * Log everything to a file if the user specified a log file path
      appLoggers :: (MonadLsp config m) => LogAction m (WithSeverity Text)
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
              appEnv <- initialize appLoggers startupLoggers env
              t1 <- Time.getPOSIXTime
              let startupTime = t1 - t0
              startupLoggers <& ("Server initialized in: " <> tshow startupTime) `WithSeverity` L.Info
              pure (Right (env, appEnv)),
            staticHandlers = \_caps -> handlersWithDiagnostics,
            interpretHandler = \(env, appEnv) -> Iso {forward = (runAppM appEnv env), backward = liftIO},
            options = lspOptions
          }

  startupLoggers <& ("Starting server with options: " <> tshow cliOptions) `WithSeverity` L.Debug

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
    ioExcept (e :: E.IOException) = dumpCrash "server crashed" (E.toException e) >> return 1
    someExcept (e :: E.SomeException) = dumpCrash "server crashed" e >> return 1

initialize :: AppLogger -> LogAction IO (WithSeverity Text) -> LanguageContextEnv Config -> IO AppData
initialize appLogger _startupLogger env = do
  searchResult <- liftIO $ X.findRefsFromGit Util.searchOpts
  conn <- Db.new

  repoRootDir <- Git.getRepoRoot
  state <-
    newMVar
      AppState
        { filesWithDiagnostics = Set.empty,
          fileVersions = SM.empty,
          shouldHandleFiles = SM.empty,
          isDbDirty = False
        }
  let appData =
        AppData
          { env =
              AppEnv
                { logger = appLogger,
                  repoRootDir = FP.splitDirectories repoRootDir,
                  logPayloads = False,
                  conn
                },
            state
          }

  runAppM appData env do
    Symbols.insertSearchResult conn repoRootDir Set.empty searchResult

  pure appData

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

-- | After each handler runs, check if there are diagnostics to send to the client, and send them if so.
handlersWithDiagnostics :: Handlers AppM
handlersWithDiagnostics =
  handlers & mapHandlers goReq goNot
  where
    goReq :: forall (a :: LSP.Method 'LSP.ClientToServer 'LSP.Request). Handler AppM a -> Handler AppM a
    goReq handler msg responder =
      flip withException exHandler do
        handler msg responder
        sendDiagnostics

    goNot :: forall (a :: LSP.Method 'LSP.ClientToServer 'LSP.Notification). Handler AppM a -> Handler AppM a
    goNot handler msg = do
      flip withException exHandler do
        handler msg
        sendDiagnostics

    -- Send a message to the client, but don't recover - let the LSP crash.
    exHandler :: SomeException -> AppM ()
    exHandler ex = do
      Log.err ("xreferee failed:\n" <> T.pack (displayException ex))

-- | Where the actual logic resides for handling requests and notifications.
handlers :: Handlers AppM
handlers =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ Util.timedNot \_msg -> do
        FileWatchers.watchRepoFiles,
      notificationHandler LSP.SMethod_TextDocumentDidOpen (Util.timedNot $ filterNot handleDidOpen),
      notificationHandler LSP.SMethod_TextDocumentDidClose \_req -> do
        -- Empty handler so we don't get these warnings in the log: `LSP: no handler for: "textDocument/didClose"`
        pure (),
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_msg -> do
        cfg <- getConfig
        Log.debugP "Configuration changed" cfg,
      notificationHandler LSP.SMethod_TextDocumentDidChange (Util.timedNot $ filterNot handleDidChange),
      requestHandler LSP.SMethod_TextDocumentPrepareRename (Util.timedReq $ filterReq handlePrepareRename),
      requestHandler LSP.SMethod_TextDocumentRename (Util.timedReq $ filterReq handleRename),
      requestHandler LSP.SMethod_TextDocumentDefinition (Util.timedReq $ filterReq handleDefinition),
      requestHandler LSP.SMethod_TextDocumentReferences (Util.timedReq $ filterReq handleReferences)
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
      whenM (Util.shouldHandleFileOrDir uri) do
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
      whenM (Util.shouldHandleFileOrDir uri) do
        handler msg

-- | Dump an exception, along with a timestamp and some context, to a crash log file on disk,
-- and to stderr.
dumpCrash :: Text -> E.SomeException -> IO ()
dumpCrash context e = do
  now <- Time.getZonedTime
  let timestamp = Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Ez" now
  let msg = "[" <> timestamp <> "] " <> unpack context <> ": " <> E.displayException e <> "\n"

  -- log to stderr
  SIO.hPutStrLn SIO.stderr msg

  -- log to file
  path <- crashLogPath
  E.catch
    (SIO.withFile path SIO.AppendMode \h -> SIO.hPutStrLn h msg)
    (\(_ :: E.SomeException) -> pure ())
  where
    -- \| Where crash dumps are written when the LSP server crashes.
    crashLogPath :: IO FilePath
    crashLogPath = do
      crashDir <- Dir.getXdgDirectory Dir.XdgCache "lsp-xreferee"
      Dir.createDirectoryIfMissing True crashDir
      pure $ crashDir FP.</> "crash.log"
