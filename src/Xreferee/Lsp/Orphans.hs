{-# OPTIONS_GHC -Wno-orphans #-}

module Xreferee.Lsp.Orphans where

import Data.Ix (Ix (..))
import Data.Ix qualified as Ix
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Language.LSP.Protocol.Types qualified as LSP
import Unsafe.Coerce qualified as Unsafe
import Xreferee.Lsp.Prelude

instance Ix LSP.UInt where
  range (lo, hi) = [lo .. hi]
  inRange (lo, hi) i = inRange (fromIntegral @_ @Word lo, fromIntegral @_ @Word hi) (fromIntegral @_ @Word i)
  index (lo, hi) i = Ix.index (fromIntegral @_ @Word lo, fromIntegral @_ @Word hi) (fromIntegral @_ @Word i)

instance ToField LSP.NormalizedUri where
  toField = toField . getUri . LSP.fromNormalizedUri

instance FromField LSP.NormalizedUri where
  fromField f = unsafeMkNormalizedUri <$> fromField f
    where
      -- We only store normalized URIs in the database,
      -- so it's safe to re-construct a NormalizedUri directly from the stored Text without
      -- going through normalization again.
      --
      -- We use the `hash` function from `hashable`, just like the original
      -- implementation of `toNormalizedUri`:
      -- https://hackage-content.haskell.org/package/lsp-types-2.4.0.0/docs/src/Language.LSP.Protocol.Types.Uri.html#toNormalizedUri
      unsafeMkNormalizedUri :: Text -> NormalizedUri
      unsafeMkNormalizedUri uri = LSP.NormalizedUri (hash uri) uri

instance ToField LSP.UInt where
  toField n = toField $ Unsafe.unsafeCoerce @LSP.UInt @Word n

instance FromField LSP.UInt where
  fromField f = Unsafe.unsafeCoerce @Word @LSP.UInt <$> fromField f
