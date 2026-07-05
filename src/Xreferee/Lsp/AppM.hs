{-# LANGUAGE TemplateHaskell #-}

module Xreferee.Lsp.AppM where

import ClassyPrelude
import Colog.Core (LogAction (..), WithSeverity (..))
import Control.Lens
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Aeson qualified as J
import Data.Map.Strict qualified as SM
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Server as LSP
import Xreferee.Lsp.TH (classyIdRules)
import Xreferee.Lsp.Types (Symbols)

-- | The "escape hatch" out of `AppM`'s `StateT` layer.
--
-- `AppM = StateT AppState AppM'`, so `AppM'` is exactly `AppM`'s base monad. Anything that
-- needs `MonadLsp` (e.g. `registerCapability`) can't run directly in `AppM`, because
-- `MonadLsp` implies `MonadUnliftIO`, which `StateT` can't provide. Such code should be
-- written against `AppM'` (or, more generally, `(MonadReader r m, HasAppEnv r, MonadLsp
-- config m) => m`) and lifted into `AppM` with `lift`.
type AppM' = ReaderT AppEnv (LspM Config)

type AppM = StateT AppState AppM'

type AppLogger = forall m config. (MonadLsp config m) => LogAction m (WithSeverity Text)

runAppM :: AppState -> AppEnv -> LanguageContextEnv Config -> AppM a -> IO (a, AppState)
runAppM appState appEnv env act = do
  act
    & flip runStateT appState
    & flip runReaderT appEnv
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

-- | The per-session data the `lsp` library hands back to `interpretHandler` on every request.
--
-- `AppState` is reachable via `env.stateVar`, so there's no need for a separate field here.
newtype AppData = AppData
  { env :: AppEnv
  }

----------------------------------------------------------------------------
-- AppEnv
----------------------------------------------------------------------------

data AppEnv = AppEnv
  { logger :: AppLogger,
    -- | The current working directory, split with `splitDirectories`.
    repoRootDir :: [FilePath],
    -- | Whether to log the payloads of LSP requests / notifications
    -- (at the debug level, to the logfile, if one is supplied).
    -- Can be very verbose e.g. when a large file is opened.
    logPayloads :: Bool,
    -- | Handle to the same `MVar` that guards `AppState` across requests (see
    -- `interpretHandler` in "Xreferee.Lsp"). Only needed for out-of-band access to
    -- `AppState`, from code that runs outside of the normal per-request `StateT`
    -- threading (e.g. a dynamically-registered LSP capability handler, which is invoked
    -- later by the `lsp` library, not nested within the `AppM` computation that registered it).
    stateVar :: MVar AppState
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
