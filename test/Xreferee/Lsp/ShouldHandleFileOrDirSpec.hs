{-# LANGUAGE MultilineStrings #-}

module Xreferee.Lsp.ShouldHandleFileOrDirSpec where

import ClassyPrelude
import Control.Monad (fail)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Exception (ExitCode (..))
import GHC.Stack (HasCallStack)
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import System.IO qualified as IO
import System.Process qualified as Process
import Test.Syd
import Xreferee.Lsp.Util (ShouldHandle (..), doShouldHandleFileOrDir)

spec :: Spec
spec = describe "shouldHandleFileOrDir" $ do
  it "should handle files and directories" $ do
    withGitRepo
      """
      tracked-ignored.md
      untracked-ignored.md
      non-existent-ignored.md
      dir-empty-ignored
      dir-nonempty-untracked-ignored
      dir-nonempty-tracked-ignored
      """
      [ (Tracked, "tracked-ignored.md", "content"),
        (Tracked, "tracked.md", "content"),
        (Tracked, "tracked-binary.md", "\0"),
        (Tracked, "tracked-empty.md", ""),
        (Untracked, "untracked-ignored.md", "content"),
        (Untracked, "untracked-binary.md", "\0"),
        (Untracked, "untracked.md", "content"),
        (Tracked, "dir-with-binary/binary.md", "\0"),
        -- dirs
        (Untracked, "dir-nonempty-untracked/a.md", "content"),
        (Tracked, "dir-nonempty-tracked/a.md", "content"),
        (Untracked, "dir-nonempty-untracked-ignored/a.md", "content"),
        (Tracked, "dir-nonempty-tracked-ignored/a.md", "content"),
        --
        (Tracked, "-file.md", "")
      ]
      do
        -- tracked and ignored
        check "tracked-ignored.md" DoHandle
        -- tracked and not-ignored (text file)
        check "tracked.md" DoHandle
        -- tracked and not-ignored (binary file)
        check "tracked-binary.md" (DontHandle "binary file")
        -- tracked and not-ignored (empty file)
        check "tracked-empty.md" DoHandle

        -- untracked and ignored
        check "untracked-ignored.md" (DontHandle "untracked & git-ignored")
        -- untracked and not ignored
        check "untracked.md" DoHandle
        -- untracked and not ignored
        check "untracked-binary.md" (DontHandle "binary file")

        -- does not exist
        check "non-existent.md" (DontHandle "does not exist")
        check "non-existent-ignored.md" (DontHandle "does not exist")
        check "non-existent-dir/file.md" (DontHandle "does not exist")

        -- is directory (empty, untracked)
        Dir.createDirectory "dir-empty"
        check "dir-empty" DoHandle
        -- is directory (empty, untracked, ignored)
        Dir.createDirectory "dir-empty-ignored"
        check "dir-empty-ignored" (DontHandle "untracked & git-ignored")

        -- is directory (non empty, untracked)
        check "dir-nonempty-untracked" DoHandle
        check "dir-nonempty-untracked/a.md" DoHandle
        -- is directory (non empty, tracked)
        check "dir-nonempty-tracked" DoHandle
        check "dir-nonempty-tracked/a.md" DoHandle
        -- is directory (not empty, untracked, ignored)
        check "dir-nonempty-untracked-ignored" (DontHandle "untracked & git-ignored")
        check "dir-nonempty-untracked-ignored/a.md" (DontHandle "untracked & git-ignored")
        -- is directory (not empty, tracked, ignored)
        check "dir-nonempty-tracked-ignored" DoHandle
        check "dir-nonempty-tracked-ignored/a.md" DoHandle

        -- binary file in a directory
        check "dir-with-binary/binary.md" (DontHandle "binary file")
        check "dir-with-binary" DoHandle

        --  is the repo root
        check "." DoHandle
        --  is outside the repo root (is file)
        check "../git.log" (DontHandle "outside git repo")
        --  is outside the repo root (is directory)
        check ".." (DontHandle "outside git repo")
        --  is outside the repo root (is invalid path)
        check "../non-existent.md" (DontHandle "does not exist")
        --  is outside the repo root, but is a file tracked by ANOTHER git repo.
        fileFromThisRepo <- Dir.makeAbsolute "tracked.md"
        withGitRepo "" [] $ do
          check fileFromThisRepo (DontHandle "outside git repo")

        --  is .git folder
        check ".git" (DontHandle "in .git dir")
        check ".git/HEAD" (DontHandle "in .git dir")
        check ".gitignore" DoHandle

        -- run from a subdirectory
        Dir.withCurrentDirectory "dir-nonempty-tracked" do
          check "a.md" DoHandle
          check "../untracked-ignored.md" (DontHandle "untracked & git-ignored")
          check "../tracked-ignored.md" DoHandle
          check "../untracked.md" DoHandle
          check "../tracked.md" DoHandle

        -- files/dirs beginning with a dash
        check "-file.md" DoHandle
        Dir.createDirectory "-dir"
        check "-dir" DoHandle

        -- symlinks
        Process.callProcess "ln" ["-s", "tracked.md", "symlink.md"]
        check "symlink.md" (DontHandle "symlink")
  where
    check :: (HasCallStack) => String -> ShouldHandle -> IO ()
    check path expected = context path $ doShouldHandleFileOrDir path `shouldReturn` expected

data GitFileState
  = Tracked
  | Untracked
  deriving stock (Show, Eq)

-- | Create a temporary git repo whose files are in the given states.
withGitRepo :: String -> [(GitFileState, FilePath, Text)] -> IO a -> IO a
withGitRepo gitignore files action = do
  withSystemTempDirectory "git.XXXX" $ \tmpdir -> do
    let gitdir = tmpdir </> "repo"
        gitlog = tmpdir </> "git.log"
        git = runGit gitlog
    Dir.createDirectoryIfMissing True gitdir
    Dir.withCurrentDirectory gitdir . captureLogs gitlog $ do
      git ["init"]
      -- Write every file to disk.
      forM_ files $ \(_, relpath, content) -> do
        let fp = gitdir </> relpath
        Dir.createDirectoryIfMissing True (FP.takeDirectory fp)
        T.writeFile fp content

      -- Commit the tracked files *before* any .gitignore exists, so that
      -- TrackedIgnored files stay tracked once they are later ignored.
      let trackedPaths = [p | (st, p, _) <- files, st == Tracked]
      forM_ trackedPaths $ \p -> git ["add", "--", p]
      git ["commit", "-m", "Initial commit", "--allow-empty", "--no-verify"]

      -- Now ignore the ignored files.
      let gitignore' = T.pack gitignore
      T.writeFile (gitdir </> ".gitignore") gitignore'
      git ["add", "--", ".gitignore"]
      git ["commit", "-m", "Add .gitignore", "--no-verify"]

      -- Run the test action
      action
  where
    captureLogs logFile f = f `onException` (T.readFile logFile >>= putStrLn)

runGit :: FilePath -> [String] -> IO ()
runGit logFile args = do
  (code, stdout, stderr) <- Process.readProcessWithExitCode "git" args ""
  IO.appendFile logFile stdout
  IO.appendFile logFile stderr
  case code of
    ExitSuccess -> pure ()
    ExitFailure n ->
      fail $ "command exited with code " <> show n <> ": " <> show ("git" : args)
