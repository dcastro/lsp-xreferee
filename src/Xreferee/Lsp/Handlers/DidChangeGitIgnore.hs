module Xreferee.Lsp.Handlers.DidChangeGitIgnore where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.Set qualified as Set
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LFS
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Language.LSP.VFS qualified as VFS
import System.FilePath qualified as FP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Symbols qualified as Symbols
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
  -- .gitignore changed, so we need to clear the `shouldHandleFiles` cache.
  modifyState2 \appState ->
    AppState
      { symbols = appState.symbols,
        fileVersions = appState.fileVersions,
        -- Changes done to `.gitignore` invalidate the `shouldHandleFiles` cache
        shouldHandleFiles = mempty,
        filesWithDiagnostics = appState.filesWithDiagnostics,
        isDbDirty = appState.isDbDirty
      }

  -- Delete all symbols from the db, except for files currently open in the editor.
  openFiles <- truncateDb

  -- Load all symbols from disk
  repoRootDir <- view repoRootDir
  searchResult <- liftIO $ X.findRefsFromGit Util.searchOpts

  conn <- view conn
  Symbols.insertSearchResult conn (FP.joinPath repoRootDir) (Set.fromList openFiles) searchResult

-- | Delete every symbol from the db, except for files currently open in the editor.
-- We want to keep their symbols in the db,
-- because they might have unsaved changes
truncateDb :: AppM [LSP.Uri]
truncateDb = do
  -- Get the open files
  vfs <- lift LSP.getVirtualFiles
  let openUris = vfs ^.. VFS.vfsMap . itraversed . VFS._Open . asIndex . to LFS.fromNormalizedUri

  -- Since .gitignore has changed, we need to re-evaluate which files we should handle.
  openUris <- filterM Util.shouldHandleFileOrDir openUris
  conn <- view conn
  Db.deleteSymbolsExcept conn openUris

  pure openUris
