module Xreferee.Lsp.Handlers.OnConfigChange where

import Language.LSP.Server qualified as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Git qualified as Git
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

-- | Handle `workspace/didChangeConfiguration` notifications.
onConfigChange :: Config -> AppM ()
onConfigChange cfg = do
  state <- getState

  -- If the `ignore` setting has changed, we need to reload all symbols.
  cfg <-
    if state.lastConfig.ignore == cfg.ignore
      then do
        pure cfg
      else do
        -- Check if the `ignore` setting contains well formed git pathspecs.
        res <-
          liftIO $
            Git.lsFiles
              []
              [ "--full-name",
                "-z"
              ]
              (cfg.ignore <&> \ignore -> ":" <> unpack ignore)

        if res == Nothing
          then do
            -- If it contains malformed pathspecs, revert to the last well known good configuration.
            Log.err "The 'xreferee.ignore' configuration setting contains invalid git pathspecs."
            cfg <- pure cfg {ignore = state.lastConfig.ignore}
            LSP.setConfig cfg
            pure cfg
          else do
            -- Reload all symbols.
            Log.debugP "xreferee.ignore changed, reloading all symbols" cfg.ignore
            Symbols.reloadAllSymbols
            pure cfg

  modifyState \appState -> appState {lastConfig = cfg}
