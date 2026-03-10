{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- This is an example language server built with haskell-lsp using a 'Reactor'
-- design. With a 'Reactor' all requests are handled on a /single thread/.
-- A thread is spun up for it, which repeatedly reads from a 'TChan' of
-- 'ReactorInput's.
-- The `lsp` handlers then simply pass on all the requests and
-- notifications onto the channel via 'ReactorInput's.
-- This way there is the option of executing requests on multiple threads, without
-- blocking server communication.
--
-- To try out this server, install it with
-- > cabal install lsp-demo-reactor-server -fdemo
-- and plug it into your client of choice.
module Lib where

import Colog.Core (LogAction (..), Severity (..), WithSeverity (..), (<&))
import Colog.Core qualified as L
import Control.Concurrent
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM qualified as STM
import Control.Exception qualified as E
import Control.Lens hiding (Iso)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Control.Monad.STM
import Data.Aeson qualified as J
import Data.Int (Int32)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.LSP.Diagnostics
import Language.LSP.Logging (defaultClientLogger)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Prettyprinter
import System.Directory qualified as Dir
import System.Exit
import System.IO
import XReferee.SearchResult (SearchResult (..))
import XReferee.SearchResult qualified as X

-- ---------------------------------------------------------------------
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- ---------------------------------------------------------------------
--

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

data AppState = AppState
  { result :: SearchResult
  }
  deriving stock (Show)

type AppM = ReaderT (TVar AppState) (LspM Config)

type AppLogger = LogAction AppM (WithSeverity Text)

runAppM :: AppState -> LanguageContextEnv Config -> AppM a -> IO a
runAppM appState env act = do
  stateVar <- STM.newTVarIO appState
  act
    & flip runReaderT stateVar
    & runLspT env

getState :: AppM AppState
getState = do
  stateVar <- ask
  liftIO $ STM.readTVarIO stateVar

-- ---------------------------------------------------------------------

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

run :: IO Int
run = flip E.catches handlers $ do
  logFileHandle <- do
    -- TODO: change this to a proper path
    let logFilePath = "/home/dc/Dropbox/Projects/xreferee/lsp-xreferee/log.log"
    logFileHandle <- openFile logFilePath AppendMode
    hSetBuffering logFileHandle NoBuffering
    newMVar logFileHandle

  let -- Three loggers:
      -- 1. To stderr (shows up in the "Output" panel in vscode)
      -- 2. To the client (shows up as a user notification, filtered by severity)
      -- 3. To both
      stderrLogger :: LogAction IO (WithSeverity Text)
      stderrLogger = L.cmap show L.logStringStderr

      clientLogger :: (MonadLsp Config m) => LogAction m (WithSeverity Text)
      clientLogger = defaultClientLogger

      fileLogger :: LogAction IO (WithSeverity Text)
      fileLogger = LogAction $ \msg -> withMVar logFileHandle $ \logFileHandle -> hPutStrLn logFileHandle (T.unpack $ getMsg msg)

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
            configSection = "demo",
            doInitialize = \env _initializeMsg -> do
              result <- initialize fileLogger
              let appState = AppState {result}
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

initialize :: LogAction IO (WithSeverity Text) -> IO SearchResult
initialize logger = do
  result <- liftIO (X.findRefsFromGit searchOpts)
  logger <& ("Search results: " <> (T.pack $ show result)) `WithSeverity` Info
  pure result

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
      LSP._save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions,
      -- TODO: review this
      optExecuteCommandCommands = Just ["lsp-hello-command"]
    }

-- ---------------------------------------------------------------------

-- The reactor is a process that serialises and buffers all requests from the
-- LSP client, so they can be sent to the backend compiler one at a time, and a
-- reply sent.

newtype ReactorInput
  = ReactorAction (IO ())

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: LSP.NormalizedUri -> Maybe Int32 -> AppM ()
sendDiagnostics fileUri version = do
  let diags =
        [ LSP.Diagnostic
            (LSP.Range (LSP.Position 0 1) (LSP.Position 0 5))
            (Just LSP.DiagnosticSeverity_Warning) -- severity
            Nothing -- code
            Nothing
            (Just "lsp-hello") -- source
            "Example diagnostic message"
            Nothing -- tags
            (Just [])
            Nothing
        ]
  publishDiagnostics 100 fileUri version (partitionBySource diags)

-- ---------------------------------------------------------------------

-- | Where the actual logic resides for handling requests and notifications.
handle :: AppLogger -> Handlers AppM
handle logger =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_msg -> do
        logger <& "Processing the Initialized notification" `WithSeverity` Info

        -- We're initialized! Lets send a showMessageRequest now
        let params =
              LSP.ShowMessageRequestParams
                LSP.MessageType_Warning
                "What's your favourite language extension?"
                (Just [LSP.MessageActionItem "Rank2Types", LSP.MessageActionItem "NPlusKPatterns"])

        void $ sendRequest LSP.SMethod_WindowShowMessageRequest params $ \res ->
          case res of
            Left e -> logger <& ("Got an error: " <> T.pack (show e)) `WithSeverity` Error
            Right _ -> do
              sendNotification LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams LSP.MessageType_Info "Excellent choice")

              -- We can dynamically register a capability once the user accepts it
              sendNotification LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams LSP.MessageType_Info "Turning on code lenses dynamically")

              let regOpts = LSP.CodeLensRegistrationOptions (LSP.InR LSP.Null) Nothing (Just False)

              void
                $ registerCapability
                  mempty
                  LSP.SMethod_TextDocumentCodeLens
                  regOpts
                $ \_req responder -> do
                  logger <& "Processing a textDocument/codeLens request" `WithSeverity` Info
                  let cmd = LSP.Command "Say hello" "lsp-hello-command" Nothing
                      rsp = [LSP.CodeLens (LSP.mkRange 0 0 0 100) (Just cmd) Nothing]
                  responder (Right $ LSP.InL rsp),
      notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        logger <& ("Processing DidOpenTextDocument for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendDiagnostics (LSP.toNormalizedUri doc) (Just 0),
      notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \msg -> do
        cfg <- getConfig
        logger L.<& ("Configuration changed: " <> T.pack (show (msg, cfg))) `WithSeverity` Info
        sendNotification LSP.SMethod_WindowShowMessage $
          LSP.ShowMessageParams LSP.MessageType_Info $
            "Wibble factor set to " <> T.pack (show cfg.wibbleFactor),
      notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do
        let doc =
              msg
                ^. LSP.params
                  . LSP.textDocument
                  . LSP.uri
                  . to LSP.toNormalizedUri
        logger <& ("Processing DidChangeTextDocument for: " <> T.pack (show doc)) `WithSeverity` Info
        mdoc <- getVirtualFile doc
        case mdoc of
          Just (VirtualFile _version str _ _) -> do
            logger <& ("Found the virtual file: " <> T.pack (show str)) `WithSeverity` Info
          Nothing -> do
            logger <& ("Didn't find anything in the VFS for: " <> T.pack (show doc)) `WithSeverity` Info,
      notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            fileName = LSP.uriToFilePath doc
        logger <& ("Processing DidSaveTextDocument  for: " <> T.pack (show fileName)) `WithSeverity` Info
        sendDiagnostics (LSP.toNormalizedUri doc) Nothing,
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
      requestHandler LSP.SMethod_TextDocumentHover $ \req responder -> do
        logger <& "Processing a textDocument/hover request" `WithSeverity` Info
        let LSP.HoverParams _doc pos _workDone = req ^. LSP.params
            LSP.Position _l _c' = pos
            rsp = LSP.Hover ms (Just range)
            ms = LSP.InL $ LSP.mkMarkdown "Your type info here!"
            range = LSP.Range pos pos
        responder (Right $ LSP.InL rsp),
      requestHandler LSP.SMethod_TextDocumentDocumentSymbol $ \req responder -> do
        logger <& "Processing a textDocument/documentSymbol request" `WithSeverity` Info
        let LSP.DocumentSymbolParams _ _ doc = req ^. LSP.params
            loc = LSP.Location (doc ^. LSP.uri) (LSP.Range (LSP.Position 0 0) (LSP.Position 0 0))
            rsp = [LSP.SymbolInformation "lsp-hello" LSP.SymbolKind_Function Nothing Nothing Nothing loc]
        responder (Right $ LSP.InL rsp),
      requestHandler LSP.SMethod_TextDocumentDefinition (handleDefinition logger),
      requestHandler LSP.SMethod_TextDocumentCodeAction $ \req responder -> do
        logger <& "Processing a textDocument/codeAction request" `WithSeverity` Info
        let params = req ^. LSP.params
            doc = params ^. LSP.textDocument
            diags = params ^. LSP.context . LSP.diagnostics
            -- makeCommand only generates commands for diagnostics whose source is us
            makeCommand d
              | (LSP.Range s _) <- d ^. LSP.range,
                (Just "lsp-hello") <- d ^. LSP.source =
                  let title = "Apply LSP hello command:" <> head (T.lines $ d ^. LSP.message)
                      -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
                      cmd = "lsp-hello-command"
                      -- need 'file' and 'start_pos'
                      args =
                        [ J.object [("file", J.object [("textDocument", J.toJSON doc)])],
                          J.object [("start_pos", J.object [("position", J.toJSON s)])]
                        ]
                      cmdparams = Just args
                   in [LSP.Command title cmd cmdparams]
            makeCommand _ = []
            rsp = map LSP.InL $ concatMap makeCommand diags
        responder (Right $ LSP.InL rsp),
      requestHandler LSP.SMethod_WorkspaceExecuteCommand $ \req responder -> do
        logger <& "Processing a workspace/executeCommand request" `WithSeverity` Info
        let params = req ^. LSP.params
            margs = params ^. LSP.arguments

        logger <& ("The arguments are: " <> T.pack (show margs)) `WithSeverity` Debug
        responder (Right $ LSP.InL (J.Object mempty)) -- respond to the request
        void $ withProgress "Executing some long running command" (req ^. LSP.params . LSP.workDoneToken) Cancellable $ \update ->
          forM [(0 :: LSP.UInt) .. 10] $ \i -> do
            update (ProgressAmount (Just (i * 10)) (Just "Doing stuff"))
            liftIO $ threadDelay (1 * 1000000)
    ]

handleDefinition :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDefinition
handleDefinition logger = \req responder -> do
  -- TODO: Make paths absolute after xreferee is run, convert to Uri
  -- TODO: line should be 0-based, xrefcheck uses 1-based
  -- TODO: xreferee: make column 1-based, like `git grep`, but then convert to 0-based here for LSP

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri . to LSP.uriToFilePath & Maybe.fromJust
  let reqPos = req ^. LSP.params ^. LSP.position
  let reqLine = reqPos ^. LSP.line
  let reqLine' = fromIntegral @LSP.UInt @Int reqLine + 1
  let reqColumn = reqPos ^. LSP.character . to (fromIntegral @LSP.UInt @Int)

  reqPath <- liftIO $ Dir.makeRelativeToCurrentDirectory reqUri

  state <- getState
  let refMatch =
        state.result.references
          & Map.toList
          & Maybe.mapMaybe
            ( \(ref, locs) -> do
                refLoc <-
                  List.find
                    ( \loc ->
                        loc.filepath == reqPath
                          && loc.lineNum == reqLine'
                          && loc.columnRange.start - 1 <= reqColumn
                          && reqColumn <= loc.columnRange.end - 1
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
      let anchorMatch = state.result.anchors & Map.lookup anchor
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
                    (LSP.Position reqLine (fromIntegral @Int @UInt refLoc.columnRange.start - 1))
                    (LSP.Position reqLine (fromIntegral @Int @UInt refLoc.columnRange.end - 1 + 1))
            let anchorRange =
                  LSP.Range
                    (LSP.Position (fromIntegral @Int @LSP.UInt anchorLoc.lineNum - 1) (fromIntegral @Int @UInt anchorLoc.columnRange.start - 1))
                    (LSP.Position (fromIntegral @Int @LSP.UInt anchorLoc.lineNum - 1) (fromIntegral @Int @UInt anchorLoc.columnRange.end - 1 + 1))
            pure $
              LSP.DefinitionLink
                LSP.LocationLink
                  { _originSelectionRange = Just (refRange),
                    _targetUri = anchorUri,
                    _targetRange = anchorRange,
                    _targetSelectionRange = anchorRange
                  }
          responder $ Right $ LSP.InR (LSP.InL locs)

-- ---------------------------------------------------------------------
