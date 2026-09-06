module Xreferee.Lsp.Config where

import Control.Lens
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Git qualified as Git
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude

-- | Check if the current config is valid, and display an error if it isn't.
-- Modify the current configuration if it contains invalid settings.
ensureConfigIsValid ::
  (MonadReader r m, HasAppEnv r, MonadLsp Config m) =>
  -- | If the `ignore` setting is invalid, revert to this one.
  -- This value MUST be a valid setting.
  [Text] ->
  m ()
ensureConfigIsValid lastGoodIgnore = do
  cfg <- LSP.getConfig

  -- If the `ignore` setting has not changed, we don't need to validate it.
  -- If the `ignore` setting is empty, we don't need to validate it
  --   (and in fact, we shouldn't, because "git ls-file" would return ALL files in the repo).
  when (cfg.ignore /= lastGoodIgnore && cfg.ignore /= []) do
    -- Check if the `ignore` setting contains well formed git pathspecs.
    res <-
      liftIO $
        Git.lsFiles
          []
          [ "--full-name",
            "-z"
          ]
          (cfg.ignore <&> \ignore -> ":" <> unpack ignore)

    when (res == Nothing) do
      -- If it contains malformed pathspecs, use the fallback config.
      Log.err "The 'xreferee.ignore' configuration setting contains invalid git pathspecs."
      cfg <- pure cfg {ignore = lastGoodIgnore}
      LSP.setConfig cfg
