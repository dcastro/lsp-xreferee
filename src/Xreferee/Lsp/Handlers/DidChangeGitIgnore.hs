module Xreferee.Lsp.Handlers.DidChangeGitIgnore where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Control.Monad.State (execStateT, modify)
import Data.ByteString.Lazy qualified as LBS
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LFS
import Language.LSP.Server as LSP
import Language.LSP.VFS qualified as VFS
import System.FilePath qualified as FP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.SendDiagnostics (modifyState)
import Xreferee.Lsp.Types qualified as Types
import Xreferee.Lsp.Util qualified as Util

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
--
-- When any `.gitignore` file changes in the repo, we need to rebuild the symbol index for all files in the repo.
handleDidChangeGitIgnore :: Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles
handleDidChangeGitIgnore = \req -> do
  Log.logNot req
  Log.debug ".gitignore changed, reloading all symbols"
  reloadAllSymbols

reloadAllSymbols :: AppM ()
reloadAllSymbols = do
  repoRootDir <- view repoRootDir

  -- Load all symbols from disk
  searchResult <- liftIO $ X.findRefsFromGit Util.searchOpts
  let newSymbols = Types.mkSymbols (FP.joinPath repoRootDir) searchResult

  vfs <- LSP.getVirtualFiles
  -- Get only the "open" files from the virtual file system.
  -- Use an indexed optic to carry and preserve the map's keys.
  let openFiles = vfs ^@.. VFS.vfsMap . itraversed . VFS._Open

  modifyState \appState -> do
    appState <-
      pure
        AppState
          { -- Update the symbols
            symbols = newSymbols,
            -- Note: the loop below will re-populate the `fileVersions` map with the
            -- versions of the files that are currently open in the editor.
            -- I don't think it'll actually change the map's contents.
            fileVersions = appState.fileVersions,
            -- Changes done to `.gitignore` invalidate the `shouldHandleFiles` cache
            shouldHandleFiles = mempty,
            filesWithDiagnostics = appState.filesWithDiagnostics
          }

    -- For files that are currently open in the editor, we want to keep their symbols in the index,
    -- because they might have unsaved changes.
    flip execStateT appState do
      forM openFiles \(normalizedUri, vfile) -> do
        let fileVersion = VFS.virtualFileVersion vfile
        let uri = LFS.fromNormalizedUri normalizedUri
        let contents = VFS.virtualFileText vfile & encodeUtf8
        modify $ Util.loadSymbolsForFile uri (LBS.fromStrict contents) fileVersion
