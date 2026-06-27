module Xreferee.Lsp.FileWatchers where

import ClassyPrelude
import Colog.Core qualified as L
import Control.Lens
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Prettyprinter
import System.FilePath qualified as FP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Handlers.DidChangeGitIgnore qualified as Handlers
import Xreferee.Lsp.Handlers.DidChangeWatchedFiles qualified as Handlers
import Xreferee.Lsp.Log qualified as Log

watchRepoFiles :: AppM ()
watchRepoFiles = do
  repoRootDir <- view repoRootDir

  let watcher = mkFileWatcher repoRootDir "**/*"

  -- A single `workspace/didChangeWatchedFiles` registration can have only ONE handler:
  -- the `lsp` library keys dynamic registrations by method, so registering twice for the
  -- same method overwrites the first handler. Instead, we register a single handler that
  -- dispatches to the right internal handler.
  let handler = dispatcher

  appLogger <- view logger
  let coreLogger = L.cmap (fmap (tshow . pretty)) appLogger
  let registrationOptions =
        LSP.DidChangeWatchedFilesRegistrationOptions
          { _watchers = [watcher]
          }
  result <- LSP.registerCapability coreLogger LSP.SMethod_WorkspaceDidChangeWatchedFiles registrationOptions handler

  case result of
    Nothing ->
      Log.err "Failed to register workspace/didChangeWatchedFiles watcher."
    Just _token ->
      Log.info "Registered workspace/didChangeWatchedFiles watcher."

-- | Dispatch a `workspace/didChangeWatchedFiles` notification to the appropriate handler.
--
-- If any `.gitignore` file changed, we rebuild the entire symbol index via
-- 'Handlers.handleDidChangeGitIgnore' (it reloads from git, which subsumes every other
-- file event in this batch). Otherwise we handle the events incrementally.
dispatcher :: LSP.Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles
dispatcher = \req -> do
  let changes = req ^. LSP.params . LSP.changes
  if any (isGitIgnore . view LSP.uri) changes
    then Handlers.handleDidChangeGitIgnore req
    else Handlers.handleDidChangeWatchedFiles req
  where
    isGitIgnore :: LSP.Uri -> Bool
    isGitIgnore uri =
      case LSP.uriToFilePath uri of
        Nothing -> False
        Just fp -> FP.takeFileName fp == ".gitignore"

mkFileWatcher :: [FilePath] -> Text -> LSP.FileSystemWatcher
mkFileWatcher repoRootDir ptrn =
  LSP.FileSystemWatcher
    { _globPattern =
        LSP.GlobPattern $
          LSP.InR $
            LSP.RelativePattern
              { -- Watch every file in this git repo, not JUST in this workspace folder.
                -- Files in a git repo can all reference each other.
                -- If the user opens the editor in a subdirectory of the git repo, we still want to watch all files in the repo.
                _baseUri = LSP.InR $ LSP.filePathToUri $ FP.joinPath repoRootDir,
                _pattern = LSP.Pattern ptrn
              },
      _kind = Nothing
    }
