module Xreferee.Lsp.Handlers.DidChangeWatchedFilesSpec where

import Language.LSP.Protocol.Types qualified as LSP
import Xreferee.Lsp.Handlers.DidChangeWatchedFiles (FileEvent (..), dedupeEvents, mkFileEvent)
import Xreferee.Lsp.TestPrelude

spec :: Spec
spec =
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

mkCreated :: FilePath -> FileEvent
mkCreated path =
  mkFileEvent
    $ LSP.FileEvent
      { _uri = LSP.filePathToUri path,
        _type_ = LSP.FileChangeType_Created
      }

mkChanged :: FilePath -> FileEvent
mkChanged path =
  mkFileEvent
    $ LSP.FileEvent
      { _uri = LSP.filePathToUri path,
        _type_ = LSP.FileChangeType_Changed
      }

mkDeleted :: FilePath -> FileEvent
mkDeleted path =
  mkFileEvent
    $ LSP.FileEvent
      { _uri = LSP.filePathToUri path,
        _type_ = LSP.FileChangeType_Deleted
      }
