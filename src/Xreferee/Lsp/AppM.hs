module Xreferee.Lsp.AppM where

import ClassyPrelude
import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Lens hiding (Iso)
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Server as LSP
import Xreferee.Lsp.Types (Symbols)

type AppM = ReaderT (MVar AppState) (LspM Config)

type AppLogger = LogAction AppM (WithSeverity Text)

runAppM :: MVar AppState -> LanguageContextEnv Config -> AppM a -> IO a
runAppM appState env act = do
  act
    & flip runReaderT appState
    & runLspT env

getState :: AppM AppState
getState = do
  stateVar <- ask
  liftIO $ readMVar stateVar

----------------------------------------------------------------------------
-- Config
----------------------------------------------------------------------------

data Config = Config {fooTheBar :: Bool, wibbleFactor :: Int}
  deriving stock (Generic, Show)
  deriving anyclass (J.ToJSON, J.FromJSON)

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
