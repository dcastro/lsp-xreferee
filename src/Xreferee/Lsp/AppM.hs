module Xreferee.Lsp.AppM where

import ClassyPrelude
import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Lens hiding (Iso)
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Server as LSP
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

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
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
    fileVersions :: SM.Map Uri Int32
  }
  deriving stock (Show)
