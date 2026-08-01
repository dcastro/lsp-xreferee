module Xreferee.Lsp.DbSpec where

import Data.Set qualified as Set
import Language.LSP.Protocol.Types qualified as LSP
import Xreferee.Lsp.AppM (AppData (..), AppEnv (..), AppState (..))
import Xreferee.Lsp.Db (Symbol (..))
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.TestPrelude

spec :: Spec
spec =
  describe "database" do
    it "findFilesInPathWithSymbols" do
      runDb do
        Db.insertAnchors
          [ mkSymbol "foo" "file1",
            mkSymbol "foo" "file12",
            mkSymbol "foo" "file1/file"
          ]
        Db.findFilesInPathWithSymbols (LSP.filePathToUri "file1")
          `shouldReturn` Set.fromList [LSP.filePathToUri "file1", LSP.filePathToUri "file1/file"]

mkSymbol :: Text -> Text -> Symbol
mkSymbol name path =
  Symbol
    { name = name,
      uri = LSP.filePathToUri (unpack path),
      line = Db.LineNum 0,
      columnStart = 0,
      columnEnd = 0
    }

runDb :: ReaderT AppData IO a -> IO a
runDb act = do
  conn <- Db.new

  state <-
    newMVar
      AppState
        { filesWithDiagnostics = mempty,
          shouldHandleFiles = mempty,
          isDbDirty = False
        }

  let appData =
        AppData
          { env =
              AppEnv
                { logger = mempty,
                  repoRootDir = ".",
                  logPayloads = False,
                  conn
                },
            state
          }

  act
    & flip runReaderT appData
