module Xreferee.Lsp.TestPrelude
  ( module M,
    shouldReturn,
    mkUri,
  )
where

import GHC.Stack (HasCallStack)
import Language.LSP.Protocol.Types qualified as LSP
import Test.Syd as M hiding (shouldReturn)
import Test.Syd qualified as Syd
import Xreferee.Lsp.Prelude as M

shouldReturn :: (HasCallStack, Show a, Eq a, MonadUnliftIO m) => m a -> a -> m ()
shouldReturn action expected =
  withRunInIO \runInIO -> do
    runInIO action `Syd.shouldReturn` expected

mkUri :: FilePath -> NormalizedUri
mkUri = LSP.toNormalizedUri . LSP.filePathToUri
