module Xreferee.Lsp.Git where

import ClassyPrelude
import System.Exit (ExitCode (..))
import System.Process qualified as P

-- | Check if a file is ignored by git.
-- This is used to avoid indexing files that are not relevant to the project.
checkIgnore :: FilePath -> IO Bool
checkIgnore filePath = do
  -- NOTE: using `P.rawSystem` was causing vscode to tell the LSP server to shut down when opening an ignored file.
  (exitCode, _, _) <- P.readProcessWithExitCode "git" ["check-ignore", filePath] ""
  pure $ exitCode == ExitSuccess
