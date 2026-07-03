module Xreferee.Lsp.Types where

import ClassyPrelude
import Control.Monad.State
import Data.IxSet.Typed (Indexable (..), IxSet)
import Data.IxSet.Typed qualified as Ix
import Data.Kind (Type)
import Data.Map qualified as Map
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as Lsp
import XReferee.SearchResult (SearchResult (..))
import XReferee.SearchResult qualified as X

-- | A symbols table, built from xreferee's `XReferee.SearchResult`, except:
--    * We use `file://` URIs with absolute paths instead of relative file paths
--    * 0-based line and column numbers instead of 1-based.
--
-- This makes it easier to work with the LSP interface.
data Symbols = Symbols
  { anchors :: SymbolSet X.Anchor,
    references :: SymbolSet X.Reference
  }
  deriving stock (Show, Eq, Generic)

instance NFData Symbols

instance Semigroup Symbols where
  result1 <> result2 =
    Symbols
      { anchors = Ix.union result1.anchors result2.anchors,
        references = Ix.union result1.references result2.references
      }

instance Monoid Symbols where
  mempty = Symbols mempty mempty

-- | A set of symbols, indexed by various fields for efficient querying.
type SymbolSet symbol =
  IxSet (SymbolIxs symbol) (SymbolEntry symbol)

-- | An entry in the symbols table, representing a single symbol occurrence and its location.
data SymbolEntry symbol = SymbolEntry
  { symbol :: symbol,
    loc :: SymbolLoc
  }
  deriving stock (Show, Eq, Ord, Generic)

instance (NFData symbol) => NFData (SymbolEntry symbol)

-- | Indices for `SymbolEntry` that we can query by.
type SymbolIxs :: Type -> [Type]
type SymbolIxs symbol = '[symbol, Lsp.Uri, LineNum, ColumnStart, ColumnEnd]

-- | How to calculate the indices for each entry in the `SymbolSet`.
instance (Ord symbol) => Indexable (SymbolIxs symbol) (SymbolEntry symbol) where
  indices =
    Ix.ixList
      (Ix.ixFun (\entry -> [entry.symbol]))
      (Ix.ixFun (\entry -> [entry.loc.uri]))
      (Ix.ixFun (\entry -> [LineNum entry.loc.lineNum]))
      (Ix.ixFun (\entry -> [ColumnStart entry.loc.columnRange.start]))
      (Ix.ixFun (\entry -> [ColumnEnd entry.loc.columnRange.end]))

-- | A constraint used to avoid `IncoherentInstances` errors.
type SymbolIxsConstraint symbol =
  ( Ord symbol,
    Ix.IsIndexOf Lsp.Uri (SymbolIxs symbol),
    Ix.IsIndexOf LineNum (SymbolIxs symbol),
    Ix.IsIndexOf ColumnStart (SymbolIxs symbol),
    Ix.IsIndexOf ColumnEnd (SymbolIxs symbol)
  )

-- Newtypes for type-safe indices.
newtype LineNum = LineNum UInt
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (NFData)

newtype ColumnStart = ColumnStart UInt
  deriving stock (Show, Eq, Ord)
  deriving newtype (NFData)

newtype ColumnEnd = ColumnEnd UInt
  deriving stock (Show, Eq, Ord)
  deriving newtype (NFData)

data SymbolLoc = SymbolLoc
  { uri :: Lsp.Uri,
    lineNum :: UInt,
    columnRange :: ColumnRange
  }
  deriving stock (Show, Eq, Ord, Generic)

instance NFData SymbolLoc

data ColumnRange = ColumnRange
  { start :: UInt,
    end :: UInt
  }
  deriving stock (Show, Eq, Ord, Generic)

instance NFData ColumnRange

mkSymbols :: FilePath -> SearchResult -> Symbols
mkSymbols repoRootDir sr =
  Symbols
    { anchors,
      references
    }
  where
    (anchors, references) =
      flip evalState mempty $
        (,) <$> mkIxSet repoRootDir (sr.anchors) <*> mkIxSet repoRootDir (sr.references)

-- | An internal cache used during `mkSymbols` to avoid repeatedly converting the same file paths to URIs.
-- `Lsp.filePathToUri` is a relatively expensive operation.
--
-- The `xreferee` repo was used to stress test this.
-- It has 19260 anchors and 15901 references across 24 files.
-- The handler for SMethod_Initialized went from taking 4.2s to 1.6s.
type UriCache = Map FilePath Lsp.Uri

mkIxSet :: forall symbol. FilePath -> (Ord symbol) => Map symbol [X.LabelLoc] -> State UriCache (SymbolSet symbol)
mkIxSet repoRootDir map = do
  entries <-
    forM (Map.toList map) \(symbol, locs) -> do
      mkSymbolEntry repoRootDir symbol locs

  pure $ Ix.fromList (mconcat entries)

mkSymbolEntry :: forall symbol. FilePath -> symbol -> [X.LabelLoc] -> State UriCache [SymbolEntry symbol]
mkSymbolEntry repoRootDir sym locs =
  forM locs \loc -> do
    loc <- mkSymbolLoc repoRootDir loc
    pure $ SymbolEntry {symbol = sym, loc}

mkSymbolLoc :: FilePath -> X.LabelLoc -> State UriCache SymbolLoc
mkSymbolLoc repoRootDir l = do
  uri <- convertFilePathToUri repoRootDir l.filepath
  pure
    SymbolLoc
      { uri,
        lineNum = xToLsp l.lineNum,
        columnRange = mkColumnRange l.columnRange
      }

convertFilePathToUri :: FilePath -> FilePath -> State UriCache Lsp.Uri
convertFilePathToUri repoRootDir fp = do
  cache <- get
  case Map.lookup fp cache of
    Just uri -> pure uri
    Nothing -> do
      -- The paths returned by `xrefcheck` are relative to the git repo root,
      -- so we have to prepend the repo root to get an absolute path, which we then convert to a `file://` URI.
      let uri = Lsp.filePathToUri $ repoRootDir </> fp
      modify (Map.insert fp uri)
      pure uri

mkColumnRange :: X.ColumnRange -> ColumnRange
mkColumnRange cr =
  ColumnRange
    { start = xToLsp cr.start,
      end = xToLsp cr.end
    }

-- Xreferee uses 1-based lines/columns, but LSP uses 0-based lines/columns.
xToLsp :: Int -> UInt
xToLsp xLine = fromIntegral @Int @UInt (xLine - 1)
