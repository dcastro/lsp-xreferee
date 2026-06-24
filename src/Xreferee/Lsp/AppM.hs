{-# LANGUAGE TemplateHaskell #-}

module Xreferee.Lsp.AppM where

import ClassyPrelude
import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Lens
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Server as LSP
import UnliftIO qualified as Unlift
import Xreferee.Lsp.TH (classyIdRules)
import Xreferee.Lsp.Types (Symbols)

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

data Config = Config {}
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

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
    -- | The current working directory, split with `splitDirectories`.
    repoRootDir :: [FilePath]
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
  { symbols :: Symbols,
    -- | Keep track of which files have warnings/errors.
    filesWithDiagnostics :: Set Uri,
    fileVersions :: SM.Map Uri Int32,
    -- | Keep track of which files are ignored, see @(ref:shouldHandleFile)
    shouldHandleFiles :: SM.Map Uri Bool
  }
  deriving stock (Show)

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
  liftIO $ readMVar appData.state

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

-- | Modify the app state, without sending diagnostics to the client.
-- This is useful for operations that we know won't change the symbols, and thus won't change the diagnostics
--
-- You should prefer @(ref:modifyState)
modifyStateWithoutDiagnostics :: (AppState -> AppM (AppState, a)) -> AppM a
modifyStateWithoutDiagnostics act = do
  env <- ask
  Unlift.modifyMVar env.state act
