module Xreferee.Lsp.Symbols where

import ClassyPrelude
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.Map qualified as Map
import Database.SQLite.Simple (Connection)
import Language.LSP.Protocol.Types qualified as LSP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM (AppM)
import Xreferee.Lsp.Db (LineNum (..))
import Xreferee.Lsp.Db qualified as Db

-- | An internal cache used during `mkSymbols` to avoid repeatedly converting the same file paths to URIs.
-- `Lsp.filePathToUri` is a relatively expensive operation.
--
-- The `xreferee` repo was used to stress test this.
-- It has 19260 anchors and 15901 references across 24 files.
-- The handler for SMethod_Initialized went from taking 4.2s to 1.6s.
type UriCache = Map FilePath LSP.Uri

insertSearchResult :: Connection -> FilePath -> X.SearchResult -> AppM ()
insertSearchResult conn repoRootDir searchResult = do
  flip evalStateT mempty do
    forM_ (Map.toList searchResult.anchors) \(anchor, locs) -> do
      forM_ locs \loc -> do
        uri <- convertFilePathToUri repoRootDir loc.filepath

        let symbol =
              Db.Symbol
                { name = X.getLabel anchor,
                  uri,
                  line = LineNum $ xToLsp loc.lineNum,
                  columnStart = xToLsp loc.columnRange.start,
                  columnEnd = xToLsp loc.columnRange.end
                }
        lift $ Db.insertAnchor conn symbol
  where
    convertFilePathToUri :: (Monad m) => FilePath -> FilePath -> StateT UriCache m LSP.Uri
    convertFilePathToUri repoRootDir fp = do
      cache <- get
      case Map.lookup fp cache of
        Just uri -> pure uri
        Nothing -> do
          -- The paths returned by `xrefcheck` are relative to the git repo root,
          -- so we have to prepend the repo root to get an absolute path, which we then convert to a `file://` URI.
          let uri = LSP.filePathToUri $ repoRootDir </> fp
          modify (Map.insert fp uri)
          pure uri

-- Xreferee uses 1-based lines/columns, but LSP uses 0-based lines/columns.
xToLsp :: Int -> LSP.UInt
xToLsp xLine = fromIntegral @Int @LSP.UInt (xLine - 1)
