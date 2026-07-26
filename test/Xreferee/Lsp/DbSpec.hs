module Xreferee.Lsp.DbSpec where

import Database.SQLite.Simple qualified as SQL
import Language.LSP.Protocol.Types qualified as LSP
import Xreferee.Lsp.AppM (AppData (..), AppEnv (..), AppState (..), HasAppEnv (conn))
import Xreferee.Lsp.Db (Symbol (..))
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.TestPrelude

spec :: Spec
spec =
  describe "database" do
    it "deleteSymbolsForFileOrDirectory" do
      runDb do
        Db.insertAnchors
          [ mkSymbol "foo" "file1",
            mkSymbol "foo" "file12",
            mkSymbol "foo" "file1/file"
          ]
        Db.deleteSymbolsForFileOrDirectory (LSP.filePathToUri "file1")
        findAllAnchors `shouldReturn` [mkSymbol "foo" "file12"]

findAllReferences :: (Db.MonadDb m) => m [Symbol]
findAllReferences = do
  conn <- view conn
  rows <- liftIO $ SQL.query_ conn "SELECT name, uri, line, column_start, column_end FROM refs"
  pure rows

findAllAnchors :: (Db.MonadDb m) => m [Symbol]
findAllAnchors = do
  conn <- view conn
  rows <- liftIO $ SQL.query_ conn "SELECT name, uri, line, column_start, column_end FROM anchors"
  pure rows

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
          fileVersions = mempty,
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
