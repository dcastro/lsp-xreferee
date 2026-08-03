module Xreferee.Lsp.Git where

import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process qualified as P
import Xreferee.Lsp.Prelude

data CheckIgnoreResult = UntrackedIgnored | NotUntrackedIgnored | OutsideRepo
  deriving stock (Show, Eq)

-- | Check if a file is untracked & ignored by git.
checkIgnore :: FilePath -> IO CheckIgnoreResult
checkIgnore filePath = do
  -- NOTE: using `P.rawSystem` was causing vscode to tell the LSP server to shut down when opening an ignored file.
  (exitCode, _, stderr) <- P.readProcessWithExitCode "git" ["check-ignore", "--", filePath] ""
  case exitCode of
    ExitSuccess -> pure UntrackedIgnored
    ExitFailure 1 -> pure NotUntrackedIgnored
    ExitFailure 128 -> pure OutsideRepo
    ExitFailure code ->
      throwIO $
        userError $
          "checkIgnore: unexpected exit code: " <> show code <> " for file '" <> filePath <> "'. stderr: " <> stderr

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

lsFiles :: [String] -> [String] -> [String] -> IO (Maybe Text)
lsFiles globalOptions options pathSpecs = do
  (exitCode, stdout, _stderr) <-
    P.readProcessWithExitCode
      "git"
      ( globalOptions
          <> [ "ls-files",
               "--others", -- Consider untracked files
               "--cached", -- Consider tracked files
               "--exclude-standard" -- Don't consider files ignored by git
             ]
          <> options
          <> ["--"]
          <> pathSpecs
      )
      ""
  case exitCode of
    ExitSuccess -> pure $ Just $ T.pack stdout
    -- This might happen if the file is outside the current repo.
    _ -> pure Nothing
