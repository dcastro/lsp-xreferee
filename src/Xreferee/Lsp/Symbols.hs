module Xreferee.Lsp.Symbols where

import Control.Lens
import Control.Monad.State (StateT, evalStateT, get, modify)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map qualified as Map
import Data.Map.Strict qualified as SM
import Data.Set qualified as Set
import Language.LSP.Protocol.Types qualified as LFS
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db (LineNum (..), Symbol (..))
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Util qualified as Util

-- | An internal cache used during `insertSearchResult` to avoid repeatedly converting the same file paths to URIs.
-- `Lsp.filePathToUri` is a relatively expensive operation.
--
-- The `xreferee` repo was used to stress test this.
-- It has 19260 anchors and 15901 references across 24 files.
-- The handler for SMethod_Initialized went from taking 4.2s to 1.6s.
type UriCache = Map FilePath LSP.Uri

insertSearchResult :: FilePath -> Set LSP.Uri -> X.SearchResult -> AppM ()
insertSearchResult repoRootDir excludedFiles searchResult = do
  (anchors, references) <- flip evalStateT mempty do
    anchors <- toSymbols searchResult.anchors
    references <- toSymbols searchResult.references
    pure (anchors, references)
  Db.insertAnchors anchors
  Db.insertReferences references
  where
    -- Converts all the labels' locations into `Symbol`s, discarding the ones in excluded files.
    toSymbols :: (X.Label label, Monad m) => Map label [X.LabelLoc] -> StateT UriCache m [Symbol]
    toSymbols labels =
      fmap concat $ forM (Map.toList labels) \(label, locs) ->
        fmap catMaybes $ forM locs \loc -> do
          uri <- convertFilePathToUri repoRootDir loc.filepath
          pure $
            if Set.member uri excludedFiles
              then Nothing
              else Just $ mkSymbol label uri (LineNum $ xToLsp loc.lineNum) loc.columnRange

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

-- | Removes the cached symbols for this file and loads the new symbols from the given file contents.
reloadSymbolsForFile :: Uri -> LByteString -> AppM ()
reloadSymbolsForFile uri contents = do
  -- Delete the old symbols for this file.
  Db.deleteSymbolsForFile uri

  -- Parse the new symbols for this file.
  let (anchors, refs) =
        foldMap
          (\(line, lineNum) -> parseLine uri (LineNum lineNum) line)
          (LBS.lines contents `zip` [0 ..])
  Db.insertAnchors anchors
  Db.insertReferences refs

-- | Parses the anchors and references found in a single line of a file.
parseLine :: LSP.Uri -> LineNum -> LByteString -> ([Symbol], [Symbol])
parseLine uri lineNum line =
  let (anchors, refs) = X.parseLabels X.defaultDelims line
      anchorSymbols = anchors <&> (\(anchor, columnRange) -> mkSymbol anchor uri lineNum columnRange)
      refSymbols = refs <&> (\(ref, columnRange) -> mkSymbol ref uri lineNum columnRange)
   in (anchorSymbols, refSymbols)

-- | Reloads all symbols from disk, clearing the cache and re-indexing all files.
--
-- This should be used when "git ignore" rules change, e.g. when `.gitignore` is edited,
-- or the user's `xreferee.ignore` setting changes.
reloadAllSymbols :: AppM ()
reloadAllSymbols = do
  oldState <- getState
  putState
    AppState
      { -- Changes done to `.gitignore` invalidate the `shouldHandleFiles` cache
        shouldHandleFiles = mempty,
        filesWithDiagnostics = oldState.filesWithDiagnostics,
        isDbDirty = oldState.isDbDirty,
        lastConfig = oldState.lastConfig
      }

  -- Delete all symbols from the db, except for files currently open in the editor.
  urisKeepSymbols <- truncateDb oldState.shouldHandleFiles
  Log.debugP "urisKeepSymbols" urisKeepSymbols

  -- Load all symbols from disk
  repoRootDir <- view repoRootDir
  cfg <- LSP.getConfig
  searchResult <- liftIO $ X.findRefsFromGit (Util.searchOpts cfg)

  insertSearchResult repoRootDir (Set.fromList urisKeepSymbols) searchResult
  where
    -- Delete every symbol from the db, except for files that:
    --   * are currently open in the editor
    --   * were being handled before `reloadAllSymbols` was called
    --   * will continue being handled after `reloadAllSymbols` is called
    --
    -- For files that meet those criteria,
    -- we want to keep their symbols in the db,
    -- because they might have unsaved changes.
    -- This function returns those files.
    --
    -- If they were being handled but aren't anymore, we want their symbols to be deleted from the db.
    -- If they weren't being handled, but will be, we want to load their symbols from disk.
    truncateDb :: SM.Map Uri Bool -> AppM [LSP.Uri]
    truncateDb oldShouldHandleFiles = do
      -- Get the open files
      vfs <- lift LSP.getVirtualFiles
      let openUris = vfs ^.. VFS.vfsMap . itraversed . VFS._Open . asIndex . to LFS.fromNormalizedUri

      -- Since .gitignore has changed, we need to re-evaluate which files we should handle.
      urisKeepSymbols <- flip filterM openUris \openUri -> do
        let wasHandling = SM.findWithDefault False openUri oldShouldHandleFiles
        willHandle <- Util.shouldHandleFileOrDir openUri
        let keepSymbols = wasHandling && willHandle
        pure keepSymbols
      Db.deleteSymbolsExcept urisKeepSymbols

      pure urisKeepSymbols

-- Xreferee uses 1-based lines/columns, but LSP uses 0-based lines/columns.
xToLsp :: Int -> LSP.UInt
xToLsp xLine = fromIntegral @Int @LSP.UInt (xLine - 1)

mkSymbol :: forall symbol. (X.Label symbol) => symbol -> LSP.Uri -> Db.LineNum -> X.ColumnRange -> Db.Symbol
mkSymbol sym uri lineNum columnRange =
  Db.Symbol
    { name = X.getLabel sym,
      uri,
      line = lineNum,
      columnStart = xToLsp columnRange.start,
      columnEnd = xToLsp columnRange.end
    }

symbolLocToLspRange :: Symbol -> LSP.Range
symbolLocToLspRange sym =
  LSP.Range
    { _start =
        LSP.Position
          { _line = sym.line.getLineNum,
            _character = sym.columnStart
          },
      _end =
        LSP.Position
          { _line = sym.line.getLineNum,
            _character = sym.columnEnd + 1
          }
    }

symbolLocToLspLocation :: Symbol -> LSP.Location
symbolLocToLspLocation sym =
  LSP.Location
    { _uri = sym.uri,
      _range = symbolLocToLspRange sym
    }
