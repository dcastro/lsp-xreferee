module Xreferee.Lsp.Types where

import Control.Lens hiding (Iso)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as Lsp
import System.FilePath ((</>))
import XReferee.SearchResult (SearchResult (..))
import XReferee.SearchResult qualified as X

-- | This is similar to `XReferee.SearchResult`, except:
--    * We use `file://` URIs with absolute paths instead of relative file paths
--    * 0-based line and column numbers instead of 1-based.
--
-- This makes it easier to work with the LSP interface.
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
    { anchors = sr.anchors <&> fmap mkLabelLoc,
      references = sr.references <&> fmap mkLabelLoc
    }
  where
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
