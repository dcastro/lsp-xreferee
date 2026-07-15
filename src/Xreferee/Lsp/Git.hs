module Xreferee.Lsp.Git where

import ClassyPrelude
import Data.Text qualified as T
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.Process qualified as P

-- | Check if a file is ignored by git.
-- This is used to avoid indexing files that are not relevant to the project.
--
-- >>> checkIgnore "README.md"
-- False
-- >>> checkIgnore "src"
-- False
-- >>> checkIgnore "invalid-path"
-- False
-- >>> checkIgnore ".stack-work"
-- True
-- >>> checkIgnore ".stack-work/file"
-- True
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

  * Will return `False` if the path is a directory.
  * Will return `False` if the path does not exist.
  * Throws if the file is outside the current git repo.

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

WARNING: the `git ls-files` command above will not list files under `.git`.

Alternative
===

Another option could be: `git grep -I --untracked --name-only -e . -- <file>`
See: https://stackoverflow.com/a/16049363/857807
`-I` tells git to ignore binary files.
`--name-only` tells git to only print the file names, not the matching lines (so returns quick even for large files)
`-e .` tells git to match any line
If it returns a non-zero exit code, the file is binary OR doesn't exist.
The issue is that it does not match on empty files! So we'd falsely report them as binary files.

>>> isBinaryFile "README.md"
False
>>> isBinaryFile ".stack-work"
False
>>> isBinaryFile "invalid-path"
False

-}
isBinaryFile :: FilePath -> IO Bool
isBinaryFile filePath = do
  -- If the path is a directory, `git ls-files` will run a deep traversal of the directory, which might be slow.
  -- So we short-circuit here.
  isDir <- Dir.doesDirectoryExist filePath
  if isDir
    then pure False
    else do
      (exitCode, stdout, _) <- P.readProcessWithExitCode "git" ["--literal-pathspecs", "ls-files", "--eol", "--others", "--cached", "--", filePath] ""
      -- NOTE: when the file doesn't exist, git will exit with code 0 and stdout will be empty.
      -- So the `isInfixOf` below will return False.
      -- This was done on purpose: we don't want to throw an exception here, the file _may_ have been deleted
      -- after the event was triggered and before we finished processing it.
      case exitCode of
        ExitSuccess -> pure $ "w/-text" `T.isInfixOf` T.pack stdout
        _ ->
          -- This might happen if the file is outside the current repo.
          throwIO $ userError $ "isBinaryFile: Failed to check if file is binary" <> stdout
