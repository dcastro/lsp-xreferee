module Xreferee.Lsp.Git where

import ClassyPrelude
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process qualified as P

data CheckIgnoreResult = UntrackedIgnored | NotUntrackedIgnored | OutsideRepo
  deriving stock (Show, Eq)

-- | Check if a file is untracked & ignored by git.
checkIgnore :: FilePath -> IO CheckIgnoreResult
checkIgnore filePath = do
  -- NOTE: using `P.rawSystem` was causing vscode to tell the LSP server to shut down when opening an ignored file.
  (exitCode, _, _) <- P.readProcessWithExitCode "git" ["check-ignore", filePath] ""
  pure $ case exitCode of
    ExitSuccess -> UntrackedIgnored
    ExitFailure 1 -> NotUntrackedIgnored
    ExitFailure _ -> OutsideRepo

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

lsFiles :: FilePath -> IO (Maybe Text)
lsFiles fp = do
  (exitCode, stdout, _stderr) <-
    P.readProcessWithExitCode
      "git"
      [ "--literal-pathspecs", -- Treat `fp` as a literal path, and not as a glob pathspec.
        "ls-files",
        "--eol", -- Print "eolinfo", which we use to determine whether a file is binary or not. See: https://stackoverflow.com/a/66796286/857807
        "--others", -- Consider untracked files
        "--cached", -- Consider tracked files
        "--exclude-standard", -- Don't consider files ignored by git
        "--",
        fp
      ]
      ""
  case exitCode of
    ExitSuccess -> pure $ Just $ T.pack stdout
    -- This might happen if the file is outside the current repo.
    _ -> pure Nothing
