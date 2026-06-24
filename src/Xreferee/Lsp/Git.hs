module Xreferee.Lsp.Git where

import ClassyPrelude
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process qualified as P

-- | Check if a file is ignored by git.
-- This is used to avoid indexing files that are not relevant to the project.
checkIgnore :: FilePath -> IO Bool
checkIgnore filePath = do
  -- NOTE: using `P.rawSystem` was causing vscode to tell the LSP server to shut down when opening an ignored file.
  (exitCode, _, _) <- P.readProcessWithExitCode "git" ["check-ignore", filePath] ""
  pure $ exitCode == ExitSuccess

-- | Get the root directory of the git repository.
--
-- This will often coincide with the editor's "workspace dir", but not always.
-- The user may open the editor in a subdirectory of the git repo.
getRepoRoot :: IO FilePath
getRepoRoot = do
  (exitCode, stdout, _) <- P.readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] ""
  case exitCode of
    ExitSuccess -> pure $ T.unpack $ T.strip $ T.pack stdout
    _ -> throwIO $ userError "Failed to get git repo root"
