{-# OPTIONS_GHC -Wno-orphans #-}

module Xreferee.Lsp.Orphans where

import ClassyPrelude
import Data.Ix (Ix (..))
import Data.Ix qualified as Ix
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.ToField (ToField (..))
import Language.LSP.Protocol.Types qualified as LSP
import Unsafe.Coerce qualified as Unsafe

instance Ix LSP.UInt where
  range (lo, hi) = [lo .. hi]
  inRange (lo, hi) i = inRange (fromIntegral @_ @Word lo, fromIntegral @_ @Word hi) (fromIntegral @_ @Word i)
  index (lo, hi) i = Ix.index (fromIntegral @_ @Word lo, fromIntegral @_ @Word hi) (fromIntegral @_ @Word i)

instance ToField LSP.Uri where
  toField uri = toField uri.getUri

instance FromField LSP.Uri where
  fromField f = LSP.Uri <$> fromField f

instance ToField LSP.UInt where
  toField n = toField $ Unsafe.unsafeCoerce @LSP.UInt @Word n

instance FromField LSP.UInt where
  fromField f = Unsafe.unsafeCoerce @Word @LSP.UInt <$> fromField f
