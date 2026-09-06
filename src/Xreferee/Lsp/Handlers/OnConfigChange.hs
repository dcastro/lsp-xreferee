module Xreferee.Lsp.Handlers.OnConfigChange where

import Language.LSP.Server qualified as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Config qualified as Config
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

-- | Handle `workspace/didChangeConfiguration` notifications.
onConfigChange :: Config -> AppM ()
onConfigChange _ = do
  state <- getState

  Config.ensureConfigIsValid
    -- If it contains malformed pathspecs, revert to the last well known good configuration.
    state.lastConfig.ignore

  cfg <- LSP.getConfig
  when (state.lastConfig.ignore /= cfg.ignore) do
    -- Reload all symbols.
    Log.debugP "xreferee.ignore changed, reloading all symbols" cfg.ignore
    Symbols.reloadAllSymbols

  modifyState \appState -> appState {lastConfig = cfg}
