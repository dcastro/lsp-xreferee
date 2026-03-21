module Xreferee.Lsp.Types where

import Control.Lens hiding (Indexable, Iso)
import Data.IxSet.Typed (Indexable (..), IxSet)
import Data.IxSet.Typed qualified as Ix
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as Lsp
import System.FilePath ((</>))
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
  deriving stock (Show, Eq)

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
    loc :: LabelLoc
  }
  deriving stock (Show, Eq, Ord)

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
newtype LineNum = LineNum UInt deriving stock (Show, Eq, Ord)

newtype ColumnStart = ColumnStart UInt deriving stock (Show, Eq, Ord)

newtype ColumnEnd = ColumnEnd UInt deriving stock (Show, Eq, Ord)

data LabelLoc = LabelLoc
  { uri :: Lsp.Uri,
    lineNum :: UInt,
    columnRange :: ColumnRange
  }
  deriving stock (Show, Eq, Ord)

data ColumnRange = ColumnRange
  { start :: UInt,
    end :: UInt
  }
  deriving stock (Show, Eq, Ord)

mkSymbols :: FilePath -> SearchResult -> Symbols
mkSymbols workspaceDir sr =
  Symbols
    { anchors = mkIxSet sr.anchors,
      references = mkIxSet sr.references
    }
  where
    mkIxSet :: forall symbol. (Ord symbol) => Map symbol [X.LabelLoc] -> SymbolSet symbol
    mkIxSet map = map & Map.toList >>= (\(symbol, locs) -> mkSymbolEntry symbol locs) & Ix.fromList

    mkSymbolEntry :: forall symbol. symbol -> [X.LabelLoc] -> [SymbolEntry symbol]
    mkSymbolEntry sym locs = locs <&> \loc -> SymbolEntry {symbol = sym, loc = mkLabelLoc loc}

    mkLabelLoc :: X.LabelLoc -> LabelLoc
    mkLabelLoc l =
      LabelLoc
        { uri = Lsp.filePathToUri $ workspaceDir </> l.filepath,
          lineNum = xToLsp l.lineNum,
          columnRange = mkColumnRange l.columnRange
        }

mkColumnRange :: X.ColumnRange -> ColumnRange
mkColumnRange cr =
  ColumnRange
    { start = xToLsp cr.start,
      end = xToLsp cr.end
    }

-- Xreferee uses 1-based lines/columns, but LSP uses 0-based lines/columns.
xToLsp :: Int -> UInt
xToLsp xLine = fromIntegral @Int @UInt (xLine - 1)
