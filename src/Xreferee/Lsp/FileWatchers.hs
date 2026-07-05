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
import Xreferee.Lsp.Util qualified as Util

watchRepoFiles :: AppM ()
watchRepoFiles = do
  repoRootDir <- view repoRootDir
  appEnv <- view appEnv
  appLogger <- view logger
  lspEnv <- lift LSP.getLspEnv

  let watcher = mkFileWatcher repoRootDir "**/*"

  -- A single `workspace/didChangeWatchedFiles` registration can have only ONE handler:
  -- the `lsp` library keys dynamic registrations by method, so registering twice for the
  -- same method overwrites the first handler. Instead, we register a single handler that
  -- dispatches to the right internal handler.
  let handlerAppM = Util.timedNot dispatcher

  -- `registerCapability` requires `MonadLsp`, which `AppM`'s `StateT` layer can't provide.
  -- We call it in `AppM'` instead (via `lift`), and bridge the registered handler back into
  -- `AppM` the same way `interpretHandler` does: acquire the state MVar, run the `StateT`
  -- computation, release the MVar. This is necessary (rather than e.g. just `lift`ing the
  -- handler) because the handler is invoked later, out-of-band, by the `lsp` library's
  -- dispatch machinery -- not nested within this `AppM` computation.
  let handler req =
        liftIO $ modifyMVar appEnv.stateVar \appState -> do
          (a, appState') <- runAppM appState appEnv lspEnv (handlerAppM req)
          pure (appState', a)

  let coreLogger = L.cmap (fmap (tshow . pretty)) appLogger
  let registrationOptions =
        LSP.DidChangeWatchedFilesRegistrationOptions
          { _watchers = [watcher]
          }
  result <-
    lift $
      LSP.registerCapability
        coreLogger
        LSP.SMethod_WorkspaceDidChangeWatchedFiles
        registrationOptions
        handler

  case result of
    Nothing ->
      lift $ Log.err "Failed to register workspace/didChangeWatchedFiles watcher."
    Just _token ->
      lift $ Log.info "Registered workspace/didChangeWatchedFiles watcher."

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
