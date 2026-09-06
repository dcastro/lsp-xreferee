module Xreferee.Lsp.Handlers.DidChangeGitIgnore where

import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
--
-- When any `.gitignore` file changes in the repo, we need to rebuild the symbol index for all files in the repo.
handleDidChangeGitIgnore :: Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles
handleDidChangeGitIgnore req = do
  annotateStackStringIO "handleDidChangeGitIgnore" do
    Log.logNot req
    Log.debug ".gitignore changed, reloading all symbols"
    Symbols.reloadAllSymbols
