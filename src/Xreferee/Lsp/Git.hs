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

{-

Checks if a file is considered binary by git.
Throws if the file does not exist or is outside the current git repo.

Implementation:
===

Runs `git ls-files --eol --others --cached -- <file>`.
If a file is binary, it'll return `w/-text` in the output.
`--others` and `--cached` ensure we check tracked, untracked, and ignored files.
See: https://stackoverflow.com/a/66796286/857807

Example output:
```
i/      w/none  attr/                   file2.md
i/      w/-text attr/                   lsp-xreferee.eventlog
i/      w/lf    attr/                   lsp-xreferee.hp
i/      w/lf    attr/                   lsp-xreferee.prof
i/none  w/none  attr/                   file.md
```

Another option could be: `git grep -I --untracked --name-only -e . -- <file>`
See: https://stackoverflow.com/a/16049363/857807
`-I` tells git to ignore binary files.
`--name-only` tells git to only print the file names, not the matching lines (so returns quick even for large files)
`-e .` tells git to match any line
If it returns a non-zero exit code, the file is binary OR doesn't exist.
The issue is that it does not match on empty files! So we'd falsely report them as binary files.

>>> isBinaryFile "README.md"
False

-}
isBinaryFile :: FilePath -> IO Bool
isBinaryFile filePath = do
  (exitCode, stdout, _) <- P.readProcessWithExitCode "git" ["ls-files", "--eol", "--others", "--cached", "--", filePath] ""
  if stdout == ""
    then throwIO $ userError $ "isBinaryFile: File does not exist: " <> filePath
    else pure ()
  case exitCode of
    ExitSuccess -> pure $ "w/-text" `T.isInfixOf` T.pack stdout
    _ ->
      -- This might happen if the file is outside the current repo.
      throwIO $ userError $ "isBinaryFile: Failed to check if file is binary" <> stdout
