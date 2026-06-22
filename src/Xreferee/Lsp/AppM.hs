module Xreferee.Lsp.AppM where

import ClassyPrelude
import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Lens hiding (Iso)
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Server as LSP
import UnliftIO qualified as Unlift
import Xreferee.Lsp.Types (Symbols)

type AppM = ReaderT AppEnv (LspM Config)

type AppLogger = LogAction AppM (WithSeverity Text)

runAppM :: AppEnv -> LanguageContextEnv Config -> AppM a -> IO a
runAppM appEnv env act = do
  act
    & flip runReaderT appEnv
    & runLspT env

getState :: AppM AppState
getState = do
  env <- ask
  liftIO $ readMVar env.state

----------------------------------------------------------------------------
-- Config
----------------------------------------------------------------------------

data Config = Config {}
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

----------------------------------------------------------------------------
-- AppEnv
----------------------------------------------------------------------------

data AppEnv = AppEnv
  { logger :: AppLogger,
    -- | The current working directory, split with `splitDirectories`.
    workspaceDir :: [FilePath],
    state :: MVar AppState
  }

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

-- | Modify the app state, without sending diagnostics to the client.
-- This is useful for operations that we know won't change the symbols, and thus won't change the diagnostics
--
-- You should prefer @(ref:modifyState)
modifyStateWithoutDiagnostics_ :: (AppState -> AppM AppState) -> AppM ()
modifyStateWithoutDiagnostics_ act = do
  env <- ask
  Unlift.modifyMVar_ env.state act

-- | Modify the app state, without sending diagnostics to the client.
-- This is useful for operations that we know won't change the symbols, and thus won't change the diagnostics
--
-- You should prefer @(ref:modifyState)
modifyStateWithoutDiagnostics :: (AppState -> AppM (AppState, a)) -> AppM a
modifyStateWithoutDiagnostics act = do
  env <- ask
  Unlift.modifyMVar env.state act
