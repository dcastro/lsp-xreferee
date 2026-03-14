module Xreferee.Lsp.AppM where

import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Concurrent
import Control.Lens hiding (Iso)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (runReaderT), ask)
import Data.Aeson qualified as J
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.LSP.Server as LSP
import UnliftIO.MVar qualified as Unlift
import XReferee.SearchResult (SearchResult (..))

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

modifyState :: (AppState -> AppM AppState) -> AppM ()
modifyState act = do
  stateVar <- ask
  Unlift.modifyMVar_ stateVar act

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
  { symbols :: SearchResult,
    -- | Keep track of which files have warnings/errors.
    filesWithDiagnostics :: Set FilePath
  }
  deriving stock (Show)
