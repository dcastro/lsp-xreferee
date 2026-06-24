module Xreferee.Lsp.FileWatchers where

import ClassyPrelude
import Colog.Core qualified as L
import Control.Lens
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Prettyprinter
import System.FilePath qualified as FP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Handlers.DidChangeWatchedFiles qualified as Handlers
import Xreferee.Lsp.Log qualified as Log

-- | Ask the client to start watching files and sending `workspace/didChangeWatchedFiles` notifications.
registerDidChangeWatchedFiles :: LSP.FileSystemWatcher -> LSP.Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles -> AppM ()
registerDidChangeWatchedFiles watcher handler = do
  let registrationOptions =
        LSP.DidChangeWatchedFilesRegistrationOptions
          { _watchers = [watcher]
          }

  appLogger <- view logger
  let coreLogger = L.cmap (fmap (tshow . pretty)) appLogger
  result <- LSP.registerCapability coreLogger LSP.SMethod_WorkspaceDidChangeWatchedFiles registrationOptions handler

  case result of
    Nothing ->
      Log.err "Failed to register workspace/didChangeWatchedFiles watcher."
    Just _token ->
      Log.info "Registered workspace/didChangeWatchedFiles watcher."

watchRepoFiles :: AppM ()
watchRepoFiles = do
  repoRootDir <- view repoRootDir
  let watcher =
        LSP.FileSystemWatcher
          { _globPattern =
              LSP.GlobPattern $
                LSP.InR $
                  LSP.RelativePattern
                    { -- Watch every file in this git repo, not JUST in this workspace folder.
                      -- Files in a git repo can all reference each other.
                      -- If the user opens the editor in a subdirectory of the git repo, we still want to watch all files in the repo.
                      _baseUri = LSP.InR $ LSP.filePathToUri $ FP.joinPath repoRootDir,
                      _pattern = LSP.Pattern "**/*"
                    },
            _kind = Nothing
          }

  registerDidChangeWatchedFiles watcher Handlers.handleDidChangeWatchedFiles
