{-# LANGUAGE QuasiQuotes #-}

module Xreferee.Lsp.Db where

import Control.Lens
import Data.Text qualified as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import System.FilePath qualified as FP
import Xreferee.Lsp.AppM (AppM, AppState (..), modifyState)
import Xreferee.Lsp.Orphans ()
import Xreferee.Lsp.Prelude

-- | A symbol (anchor or reference), built from xreferee's `XReferee.SearchResult`, except:
--    * We use `file://` URIs with absolute paths instead of relative file paths
--    * 0-based line and column numbers instead of 1-based.
--
-- This makes it easier to work with the LSP interface.
data Symbol = Symbol
  { name :: Text,
    uri :: LSP.Uri,
    line :: LineNum,
    columnStart :: LSP.UInt,
    columnEnd :: LSP.UInt
  }
  deriving stock (Show, Eq)

newtype LineNum = LineNum {getLineNum :: LSP.UInt}
  deriving newtype (Show, Eq, Ord, NFData, ToField, FromField)

instance ToRow Symbol where
  toRow (Symbol name uri line columnStart columnEnd) =
    [ toField name,
      toField uri,
      toField line,
      toField columnStart,
      toField columnEnd
    ]

instance FromRow Symbol where
  fromRow = Symbol <$> field <*> field <*> field <*> field <*> field

new :: (MonadIO m) => m Connection
new = liftIO do
  conn <- open ":memory:"
  execute_
    conn
    [sql|
      CREATE TABLE anchors (
        name TEXT,
        uri TEXT,
        line INTEGER,
        column_start INTEGER,
        column_end INTEGER
      );
    |]
  execute_
    conn
    [sql|
      CREATE TABLE refs (
        name TEXT,
        uri TEXT,
        line INTEGER,
        column_start INTEGER,
        column_end INTEGER
      );
    |]

  -- Serves the `name = ?` lookups,
  -- satisfies `ORDER BY name` and `GROUP BY name`,
  -- and lets the `NOT IN (SELECT name FROM ...)` subqueries be evaluated as index probes.
  execute_ conn [sql| CREATE INDEX idx_anchors_name ON anchors (name) |]
  execute_ conn [sql| CREATE INDEX idx_refs_name ON refs (name) |]

  -- Note: there is deliberately no index on `line` alone: no query filters by
  -- line without also filtering by uri.
  execute_ conn [sql| CREATE INDEX idx_anchors_uri_line ON anchors (uri, line) |]
  execute_ conn [sql| CREATE INDEX idx_refs_uri_line ON refs (uri, line) |]

  pure conn

insertAnchors :: Connection -> [Symbol] -> AppM ()
insertAnchors conn anchors =
  unless (null anchors) do
    liftIO $
      withTransaction conn $
        executeMany
          conn
          [sql|INSERT INTO anchors (name, uri, line, column_start, column_end) VALUES (?, ?, ?, ?, ?)|]
          anchors
    setDirty

insertReferences :: Connection -> [Symbol] -> AppM ()
insertReferences conn references =
  unless (null references) do
    liftIO $
      withTransaction conn $
        executeMany
          conn
          [sql|INSERT INTO refs (name, uri, line, column_start, column_end) VALUES (?, ?, ?, ?, ?)|]
          references
    setDirty

deleteSymbolsExcept :: Connection -> [LSP.Uri] -> AppM ()
deleteSymbolsExcept conn uris = do
  let placeholders = T.intercalate "," (replicate (length uris) "?")
  let query = "DELETE FROM anchors WHERE uri NOT IN (" <> placeholders <> ")"
  liftIO $ execute conn (Query query) uris
  checkDirty conn
  let queryRefs = "DELETE FROM refs WHERE uri NOT IN (" <> placeholders <> ")"
  liftIO $ execute conn (Query queryRefs) uris
  checkDirty conn

findAnchorsWithName :: (MonadIO m) => Connection -> Text -> m [Symbol]
findAnchorsWithName conn name = liftIO do
  query @_ @Symbol
    conn
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM anchors
      WHERE name = ?
    |]
    (Only name)

findReferencesWithName :: (MonadIO m) => Connection -> Text -> m [Symbol]
findReferencesWithName conn name = liftIO do
  query @_ @Symbol
    conn
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM refs
      WHERE name = ?
    |]
    (Only name)

findAnchorAtPosition :: (MonadIO m) => Connection -> LSP.Uri -> LSP.Position -> m (Maybe Symbol)
findAnchorAtPosition conn uri lspPos = liftIO do
  let reqLine = lspPos ^. LSP.line
  let reqColumn = lspPos ^. LSP.character
  listToMaybe
    <$> query @_ @Symbol
      conn
      [sql|
        SELECT name, uri, line, column_start, column_end
        FROM anchors
        WHERE uri = ? AND line = ? AND column_start <= ? AND column_end >= ?
        LIMIT 1
    |]
      (uri, reqLine, reqColumn, reqColumn)

findReferenceAtPosition :: (MonadIO m) => Connection -> LSP.Uri -> LSP.Position -> m (Maybe Symbol)
findReferenceAtPosition conn uri lspPos = liftIO do
  let reqLine = lspPos ^. LSP.line
  let reqColumn = lspPos ^. LSP.character
  listToMaybe
    <$> query @_ @Symbol
      conn
      [sql|
        SELECT name, uri, line, column_start, column_end
        FROM refs
        WHERE uri = ? AND line = ? AND column_start <= ? AND column_end >= ?
        LIMIT 1
    |]
      (uri, reqLine, reqColumn, reqColumn)

deleteSymbolsForFile :: Connection -> LSP.Uri -> AppM ()
deleteSymbolsForFile conn uri = do
  liftIO $ execute conn [sql|DELETE FROM anchors WHERE uri = ?|] (Only uri)
  checkDirty conn
  liftIO $ execute conn [sql|DELETE FROM refs WHERE uri = ?|] (Only uri)
  checkDirty conn

deleteSymbolsForFileOrDirectory :: Connection -> LSP.Uri -> AppM ()
deleteSymbolsForFileOrDirectory conn uri = do
  let prefix = addTrailingPathSeparator uri
  liftIO $ execute conn [sql|DELETE FROM anchors WHERE uri LIKE ?|] [prefix <> "%"]
  checkDirty conn
  liftIO $ execute conn [sql|DELETE FROM refs WHERE uri LIKE ?|] [prefix <> "%"]
  checkDirty conn
  where
    -- We MUST add a trailing path separator to a uri like `./foo`,
    -- otherwise, `./foobar/file.md LIKE ./foo%` would incorrectly be `True`.
    -- Instead, the clause should be `./foobar/file.md LIKE ./foo/%`.
    addTrailingPathSeparator :: LSP.Uri -> Text
    addTrailingPathSeparator =
      T.pack . FP.addTrailingPathSeparator . T.unpack . LSP.getUri

findUnusedAnchors :: (MonadIO m) => Connection -> m [Symbol]
findUnusedAnchors conn = liftIO do
  query_ @Symbol
    conn
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM anchors
      WHERE name NOT IN (SELECT name FROM refs)
      ORDER BY name
    |]

findBrokenReferences :: (MonadIO m) => Connection -> m [Symbol]
findBrokenReferences conn = liftIO do
  query_ @Symbol
    conn
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM refs
      WHERE name NOT IN (SELECT name FROM anchors)
      ORDER BY name
    |]

findDuplicateAnchors :: (MonadIO m) => Connection -> m [Symbol]
findDuplicateAnchors conn = liftIO do
  query_ @Symbol
    conn
    -- #(ref:duplicate-anchors-sorted)
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM anchors
      WHERE name IN (SELECT name FROM anchors GROUP BY name HAVING COUNT(*) > 1)
      ORDER BY name
    |]

deleteSymbolsInLineRange :: Connection -> LSP.Uri -> LineNum -> LineNum -> AppM ()
deleteSymbolsInLineRange conn uri startLine endLine = do
  liftIO $
    execute
      conn
      [sql|DELETE FROM anchors WHERE uri = ? AND line BETWEEN ? AND ?|]
      (uri, startLine, endLine)
  checkDirty conn
  liftIO $
    execute
      conn
      [sql|DELETE FROM refs WHERE uri = ? AND line BETWEEN ? AND ?|]
      (uri, startLine, endLine)
  checkDirty conn

shiftSymbolsAfterLine :: Connection -> LSP.Uri -> LineNum -> Int -> AppM ()
shiftSymbolsAfterLine conn uri lineNum delta = do
  when (delta /= 0) do
    liftIO $
      execute
        conn
        [sql|UPDATE anchors SET line = line + ? WHERE uri = ? AND line > ?|]
        (delta, uri, lineNum)
    checkDirty conn
    liftIO $
      execute
        conn
        [sql|UPDATE refs SET line = line + ? WHERE uri = ? AND line > ?|]
        (delta, uri, lineNum)
    checkDirty conn

----------------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------------

checkDirty :: Connection -> AppM ()
checkDirty conn = do
  affectedRows <- liftIO $ changes conn
  when (affectedRows > 0) do
    setDirty

setDirty :: AppM ()
setDirty = do
  modifyState \appState -> appState {isDbDirty = True}
