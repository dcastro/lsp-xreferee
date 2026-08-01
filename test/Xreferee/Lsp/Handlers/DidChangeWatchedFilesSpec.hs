module Xreferee.Lsp.Handlers.DidChangeWatchedFilesSpec where

import Data.Set qualified as Set
import Language.LSP.Protocol.Types qualified as LSP
import System.Directory qualified as Dir
import Xreferee.Lsp.Handlers.DidChangeWatchedFiles (FileEvent (..), dedupeEvents, listPaths, mkFileEvent)
import Xreferee.Lsp.TestPrelude

spec :: Spec
spec =
  describe "DidChangeWatchedFiles" do
    describe "dedupeEvents" do
      it "should not merge unrelated events" do
        let evts =
              [ mkCreated "path1",
                mkDeleted "path1", -- same path as above, but different event type
                mkCreated "path2", -- same event type as first event, but different path
                -- Without @(ref:uriAddTrailingPathSeparator), this path would be considered to be within `path1`,
                -- and the event would be (incorrectly) deduped.
                mkCreated "path1long/file"
              ]

        dedupeEvents evts `shouldBe` (evts, [])

      it "merges equal paths" do
        dedupeEvents [mkCreated "path", mkCreated "path"] `shouldBe` ([mkCreated "path"], [mkCreated "path"])

      it "created and changed event types are equivalent" do
        dedupeEvents [mkCreated "path", mkChanged "path"] `shouldBe` ([mkCreated "path"], [mkChanged "path"])
        dedupeEvents [mkChanged "path", mkCreated "path"] `shouldBe` ([mkChanged "path"], [mkCreated "path"])

      it "deleted events are not equivalent to created/changed" do
        dedupeEvents [mkCreated "a/b", mkDeleted "a"] `shouldBe` ([mkCreated "a/b", mkDeleted "a"], [])
        dedupeEvents [mkDeleted "a", mkCreated "a/b"] `shouldBe` ([mkDeleted "a", mkCreated "a/b"], [])

      it "drops children dirs" do
        dedupeEvents [mkCreated "a", mkCreated "a/b", mkCreated "a/b/c"] `shouldBe` ([mkCreated "a"], [mkCreated "a/b", mkCreated "a/b/c"])
        dedupeEvents [mkCreated "a/b/c", mkCreated "a/b", mkCreated "a"] `shouldBe` ([mkCreated "a"], [mkCreated "a/b/c", mkCreated "a/b"])

        dedupeEvents [mkCreated "a", mkCreated "a/b", mkCreated "a/c"] `shouldBe` ([mkCreated "a"], [mkCreated "a/b", mkCreated "a/c"])
        dedupeEvents [mkCreated "a/b", mkCreated "a", mkCreated "a/c"] `shouldBe` ([mkCreated "a"], [mkCreated "a/b", mkCreated "a/c"])
        dedupeEvents [mkCreated "a/b", mkCreated "a/c", mkCreated "a"] `shouldBe` ([mkCreated "a"], [mkCreated "a/b", mkCreated "a/c"])

      it "sibling paths are not dropped" do
        dedupeEvents [mkCreated "a/b", mkCreated "a/c"] `shouldBe` ([mkCreated "a/b", mkCreated "a/c"], [])

      it "preserves order of events" do
        dedupeEvents [mkCreated "x", mkCreated "a", mkCreated "a/b", mkDeleted "y"]
          `shouldBe` ( [mkCreated "x", mkCreated "a", mkDeleted "y"],
                       [mkCreated "a/b"]
                     )
        dedupeEvents [mkCreated "a", mkCreated "x", mkCreated "a/b", mkDeleted "y"]
          `shouldBe` ( [mkCreated "a", mkCreated "x", mkDeleted "y"],
                       [mkCreated "a/b"]
                     )
        dedupeEvents [mkCreated "a/b", mkCreated "x", mkCreated "a", mkDeleted "y"]
          `shouldBe` ( [mkCreated "x", mkCreated "a", mkDeleted "y"],
                       [mkCreated "a/b"]
                     )
        dedupeEvents [mkCreated "x", mkCreated "a", mkDeleted "y", mkCreated "a/b"]
          `shouldBe` ( [mkCreated "x", mkCreated "a", mkDeleted "y"],
                       [mkCreated "a/b"]
                     )
        dedupeEvents [mkCreated "x", mkCreated "a/b", mkDeleted "y", mkCreated "a"]
          `shouldBe` ( [mkCreated "x", mkDeleted "y", mkCreated "a"],
                       [mkCreated "a/b"]
                     )
    describe "listPaths" do
      let withDirectoryTree :: (FilePath -> IO a) -> IO a
          withDirectoryTree action = do
            withSystemTempDirectory "xreferee-listPaths" \tmpDir -> do
              let dir = tmpDir </> "dir"
              Dir.createDirectory dir
              writeFile (dir </> "file1") "content"
              writeFile (dir </> "file2") "content"
              Dir.createDirectory (dir </> "subdir")
              writeFile (dir </> "subdir" </> "file3") "content"
              Dir.createDirectory (dir </> "subdir2")
              writeFile (dir </> "subdir2" </> "file4") "content"
              Dir.createDirectory (dir </> "emptysubdir")
              action tmpDir

      -- Don't short-circuit, traverse the entire directory tree
      let shouldHandle _ = pure True

      it "traverses directories" do
        withDirectoryTree \tmpDir -> do
          -- Traverses directory tree
          listPaths shouldHandle tmpDir
            `shouldReturn` Set.fromList
              [ tmpDir </> "dir" </> "file1",
                tmpDir </> "dir" </> "file2",
                tmpDir </> "dir" </> "subdir" </> "file3",
                tmpDir </> "dir" </> "subdir2" </> "file4"
              ]
          listPaths shouldHandle (tmpDir </> "dir")
            `shouldReturn` Set.fromList
              [ tmpDir </> "dir" </> "file1",
                tmpDir </> "dir" </> "file2",
                tmpDir </> "dir" </> "subdir" </> "file3",
                tmpDir </> "dir" </> "subdir2" </> "file4"
              ]
          listPaths shouldHandle (tmpDir </> "dir" </> "subdir")
            `shouldReturn` Set.fromList
              [ tmpDir </> "dir" </> "subdir" </> "file3"
              ]
          listPaths shouldHandle (tmpDir </> "dir" </> "subdir2")
            `shouldReturn` Set.fromList
              [ tmpDir </> "dir" </> "subdir2" </> "file4"
              ]

      it "handles invalid paths" do
        withDirectoryTree \tmpDir -> do
          listPaths shouldHandle (tmpDir </> "dir" </> "invalid") `shouldReturn` Set.empty

      it "handles file paths" do
        withDirectoryTree \tmpDir -> do
          listPaths shouldHandle (tmpDir </> "dir" </> "file1") `shouldReturn` Set.fromList [tmpDir </> "dir" </> "file1"]

      it "short circuits on ignored directories" do
        withDirectoryTree \tmpDir ->
          do
            listPaths (\uri -> pure $ uri /= LSP.filePathToUri (tmpDir </> "dir" </> "subdir2")) tmpDir
            `shouldReturn` Set.fromList
              [ tmpDir </> "dir" </> "file1",
                tmpDir </> "dir" </> "file2",
                tmpDir </> "dir" </> "subdir" </> "file3"
              ]

      it "excludes ignored files" do
        withDirectoryTree \tmpDir ->
          do
            listPaths (\uri -> pure $ uri /= LSP.filePathToUri (tmpDir </> "dir" </> "file1")) tmpDir
            `shouldReturn` Set.fromList
              [ tmpDir </> "dir" </> "file2",
                tmpDir </> "dir" </> "subdir" </> "file3",
                tmpDir </> "dir" </> "subdir2" </> "file4"
              ]

mkCreated :: FilePath -> FileEvent
mkCreated path =
  mkFileEvent $
    LSP.FileEvent
      { _uri = LSP.filePathToUri path,
        _type_ = LSP.FileChangeType_Created
      }

mkChanged :: FilePath -> FileEvent
mkChanged path =
  mkFileEvent $
    LSP.FileEvent
      { _uri = LSP.filePathToUri path,
        _type_ = LSP.FileChangeType_Changed
      }

mkDeleted :: FilePath -> FileEvent
mkDeleted path =
  mkFileEvent $
    LSP.FileEvent
      { _uri = LSP.filePathToUri path,
        _type_ = LSP.FileChangeType_Deleted
      }
