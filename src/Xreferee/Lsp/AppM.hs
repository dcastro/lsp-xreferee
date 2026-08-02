{-# LANGUAGE TemplateHaskell #-}

module Xreferee.Lsp.AppM where

import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Lens
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Database.SQLite.Simple (Connection)
import Language.LSP.Server as LSP
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.TH (classyIdRules)

type AppM = ReaderT AppData (LspM Config)

type AppLogger = forall m config. (MonadLsp config m) => LogAction m (WithSeverity Text)

runAppM :: AppData -> LanguageContextEnv Config -> AppM a -> IO a
runAppM appData env act = do
  act
    & flip runReaderT appData
    & runLspT env

----------------------------------------------------------------------------
-- Config
----------------------------------------------------------------------------

data Config = Config
  { -- | Git glob specs of paths to ignore when searching for refs/anchors.
    ignore :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

emptyConfig :: Config
emptyConfig = Config {ignore = []}

----------------------------------------------------------------------------
-- AppData
----------------------------------------------------------------------------

-- | AppData = AppState + AppEnv
data AppData = AppData
  { env :: AppEnv,
    state :: MVar AppState
  }

----------------------------------------------------------------------------
-- AppEnv
----------------------------------------------------------------------------

data AppEnv = AppEnv
  { logger :: AppLogger,
    -- | The current working directory.
    repoRootDir :: FilePath,
    -- | Whether to log the payloads of LSP requests / notifications
    -- (at the debug level, to the logfile, if one is supplied).
    -- Can be very verbose e.g. when a large file is opened.
    logPayloads :: Bool,
    conn :: Connection
  }

-- `logger` is a polymorphic field, and GHC does not resolve
-- `HasField` constraints for polymorphic types, so we have to either:
--
--   * define a getter function for the logger.
--   * use the `logger` lens defined below.
--
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/hasfield.html#solving-hasfield-constraints
--
-- NOTE: we need to enable `ImpredicativeTypes` for `asks getLogger` to compile.
getLogger :: AppEnv -> AppLogger
getLogger AppEnv {logger} = logger

----------------------------------------------------------------------------
-- AppState
----------------------------------------------------------------------------

data AppState = AppState
  { -- | True if the symbols database has been modified since the last time diagnostics were sent to the client.
    isDbDirty :: Bool,
    -- | Keep track of which files have warnings/errors.
    filesWithDiagnostics :: Set Uri,
    -- | Keep track of which files are ignored, see @(ref:shouldHandleFileOrDir)
    shouldHandleFiles :: SM.Map Uri Bool
  }

----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------

mconcat
  [ makeLensesWith classyIdRules ''AppData,
    makeLensesWith classyIdRules ''AppEnv,
    makeLensesWith classyIdRules ''AppState
  ]

instance HasAppEnv AppData where
  appEnv = env

-- NOTE: We can't define `HasAppState AppData` because `AppState` is gated behind an `MVar`.
getState :: AppM AppState
getState = do
  appData <- ask
  readMVar appData.state

putState :: AppState -> AppM ()
putState newState = do
  appData <- ask
  modifyMVar_ appData.state \_ -> pure newState

modifyState :: (MonadReader AppData m, MonadUnliftIO m) => (AppState -> AppState) -> m ()
modifyState f = do
  appData <- ask
  modifyMVar_ appData.state \appState -> pure (f appState)
