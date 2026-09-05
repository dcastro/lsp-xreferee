module Xreferee.Lsp.Handlers.OnConfigChange where

import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

-- | Handle `workspace/didChangeConfiguration` notifications.
onConfigChange :: Config -> AppM ()
onConfigChange cfg = do
  state <- getState

  -- If the `ignore` setting has changed, we need to reload all symbols.
  when (state.lastConfig.ignore /= cfg.ignore) do
    Log.debugP "xreferee.ignore changed, reloading all symbols" cfg.ignore
    Symbols.reloadAllSymbols

  modifyState \appState -> appState {lastConfig = cfg}
