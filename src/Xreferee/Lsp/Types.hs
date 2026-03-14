module Xreferee.Lsp.Types where

import Control.Lens hiding (Iso)
import Data.Map (Map)
import Data.Map qualified as Map
import XReferee.SearchResult (SearchResult (..))
import XReferee.SearchResult qualified as X

data Symbols = Symbols
  { anchors :: Map X.Anchor [LabelLoc],
    references :: Map X.Reference [LabelLoc]
  }
  deriving stock (Show, Eq)

instance Semigroup Symbols where
  result1 <> result2 =
    Symbols
      { anchors = Map.unionWith (<>) result1.anchors result2.anchors,
        references = Map.unionWith (<>) result1.references result2.references
      }

instance Monoid Symbols where
  mempty = Symbols mempty mempty

data LabelLoc = LabelLoc
  { filepath :: FilePath,
    lineNum :: Int,
    columnRange :: X.ColumnRange
  }
  deriving stock (Show, Eq, Ord)

mkSymbols :: SearchResult -> Symbols
mkSymbols sr =
  Symbols
    { anchors = sr.anchors <&> fmap mkLabelLoc,
      references = sr.references <&> fmap mkLabelLoc
    }
  where
    mkLabelLoc :: X.LabelLoc -> LabelLoc
    mkLabelLoc l =
      LabelLoc
        { filepath = l.filepath,
          lineNum = l.lineNum,
          columnRange = l.columnRange
        }
