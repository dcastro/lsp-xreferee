{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Xreferee.Lsp.Db where

import ClassyPrelude
import Control.Lens
import Data.Text qualified as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField (..))
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import System.FilePath qualified as FP
import Unsafe.Coerce qualified as Unsafe

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
      )
    |]
  execute_
    conn
    [sql|
      CREATE TABLE references (
        name TEXT,
        uri TEXT,
        line INTEGER,
        column_start INTEGER,
        column_end INTEGER
      )
    |]

  execute_
    conn
    [sql|
      CREATE TABLE references (
        name TEXT,
        uri TEXT,
        line INTEGER,
        column_start INTEGER,
        column_end INTEGER
      )
    |]

  execute_ conn [sql| CREATE INDEX idx_anchors_name ON anchors (name) |]
  execute_ conn [sql| CREATE INDEX idx_anchors_uri ON anchors (uri) |]
  execute_ conn [sql| CREATE INDEX idx_anchors_line ON anchors (line) |]

  execute_ conn [sql| CREATE INDEX idx_references_name ON references (name) |]
  execute_ conn [sql| CREATE INDEX idx_references_uri ON references (uri) |]
  execute_ conn [sql| CREATE INDEX idx_references_line ON references (line) |]

  pure conn

insertAnchor :: (MonadIO m) => Connection -> Symbol -> m ()
insertAnchor conn anchor = liftIO do
  execute
    conn
    [sql|INSERT INTO anchors (name, uri, line, column_start, column_end) VALUES (?, ?, ?, ?, ?)|]
    (anchor)

insertReference :: (MonadIO m) => Connection -> Symbol -> m ()
insertReference conn reference = liftIO do
  execute
    conn
    [sql|INSERT INTO references (name, uri, line, column_start, column_end) VALUES (?, ?, ?, ?, ?)|]
    (reference)

deleteSymbolsExcept :: (MonadIO m) => Connection -> [LSP.Uri] -> m ()
deleteSymbolsExcept conn uris = liftIO do
  let placeholders = T.intercalate "," (replicate (length uris) "?")
  let query = "DELETE FROM anchors WHERE uri NOT IN (" <> placeholders <> ")"
  execute conn (Query query) uris
  let queryRefs = "DELETE FROM references WHERE uri NOT IN (" <> placeholders <> ")"
  execute conn (Query queryRefs) uris

-- findSymbolAtPosition :: (MonadIO m) => Connection -> LSP.Uri -> LSP.Position -> m (Maybe Symbol)
-- findSymbolAtPosition conn uri lspPos = liftIO do
--   let reqLine = lspPos ^. LSP.line
--   let reqColumn = lspPos ^. LSP.character
--   symbols <-
--     query @_ @Symbol
--       conn
--       [sql|
--       ( SELECT name, uri, line, column_start, column_end
--         FROM anchors
--         WHERE uri = ?1 AND line = ?2 AND column_start <= ?3 AND column_end >= ?3
--         LIMIT 1
--       )

--       UNION

--       ( SELECT name, uri, line, column_start, column_end
--         FROM references
--         WHERE uri = ?1 AND line = ?2 AND column_start <= ?3 AND column_end >= ?3
--         LIMIT 1
--       )
--     |]
--       (uri, reqLine, reqColumn)

--   pure $ listToMaybe symbols

findAnchorsWithName :: (MonadIO m) => Connection -> Text -> m [Symbol]
findAnchorsWithName conn name = liftIO do
  query @_ @Symbol
    conn
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM anchors
      WHERE name = ?1
    |]
    (Only name)

findReferencesWithName :: (MonadIO m) => Connection -> Text -> m [Symbol]
findReferencesWithName conn name = liftIO do
  query @_ @Symbol
    conn
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM references
      WHERE name = ?1
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
        WHERE uri = ?1 AND line = ?2 AND column_start <= ?3 AND column_end >= ?3
        LIMIT 1
    |]
      (uri, reqLine, reqColumn)

findReferenceAtPosition :: (MonadIO m) => Connection -> LSP.Uri -> LSP.Position -> m (Maybe Symbol)
findReferenceAtPosition conn uri lspPos = liftIO do
  let reqLine = lspPos ^. LSP.line
  let reqColumn = lspPos ^. LSP.character
  listToMaybe
    <$> query @_ @Symbol
      conn
      [sql|
        SELECT name, uri, line, column_start, column_end
        FROM references
        WHERE uri = ?1 AND line = ?2 AND column_start <= ?3 AND column_end >= ?3
        LIMIT 1
    |]
      (uri, reqLine, reqColumn)

deleteSymbolsForFile :: (MonadIO m) => Connection -> LSP.Uri -> m ()
deleteSymbolsForFile conn uri = liftIO do
  execute conn [sql|DELETE FROM anchors WHERE uri = ?|] (Only uri)
  execute conn [sql|DELETE FROM references WHERE uri = ?|] (Only uri)

deleteSymbolsForFileOrDirectory :: (MonadIO m) => Connection -> LSP.Uri -> m ()
deleteSymbolsForFileOrDirectory conn uri = liftIO do
  let prefix = addTrailingPathSeparator uri
  execute conn [sql|DELETE FROM anchors WHERE uri LIKE ?|] [prefix <> "%"]
  execute conn [sql|DELETE FROM references WHERE uri LIKE ?|] [prefix <> "%"]
  where
    -- We MUST add a trailing path separator.
    -- Otherwise, `isWithinDir ./foobar/file.md ./foo` would incorrectly be `True`.
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
      WHERE name NOT IN (SELECT name FROM references)
      ORDER BY name
    |]

findBrokenReferences :: (MonadIO m) => Connection -> m [Symbol]
findBrokenReferences conn = liftIO do
  query_ @Symbol
    conn
    [sql|
      SELECT name, uri, line, column_start, column_end
      FROM references
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

deleteSymbolsInLineRange :: (MonadIO m) => Connection -> LSP.Uri -> LineNum -> LineNum -> m ()
deleteSymbolsInLineRange conn uri startLine endLine = liftIO do
  execute
    conn
    [sql|DELETE FROM anchors WHERE uri = ? AND line BETWEEN ? AND ?|]
    (uri, startLine, endLine)
  execute
    conn
    [sql|DELETE FROM references WHERE uri = ? AND line BETWEEN ? AND ?|]
    (uri, startLine, endLine)

shiftSymbolsAfterLine :: (MonadIO m) => Connection -> LSP.Uri -> LineNum -> LSP.UInt -> m ()
shiftSymbolsAfterLine conn uri lineNum delta = liftIO do
  execute
    conn
    [sql|UPDATE anchors SET line = line + ? WHERE uri = ? AND line > ?|]
    (delta, uri, lineNum)

instance ToField LSP.Uri where
  toField uri = toField uri.getUri

instance FromField LSP.Uri where
  fromField f = LSP.Uri <$> fromField f

instance ToField LSP.UInt where
  toField n = toField $ Unsafe.unsafeCoerce @LSP.UInt @Word n

instance FromField LSP.UInt where
  fromField f = Unsafe.unsafeCoerce @Word @LSP.UInt <$> fromField f

instance FromRow Symbol where
  fromRow = Symbol <$> field <*> field <*> field <*> field <*> field
