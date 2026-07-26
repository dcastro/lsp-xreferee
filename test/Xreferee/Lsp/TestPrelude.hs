module Xreferee.Lsp.TestPrelude
  ( module M,
    shouldReturn,
  )
where

import GHC.Stack (HasCallStack)
import Test.Syd as M hiding (shouldReturn)
import Test.Syd qualified as Syd
import Xreferee.Lsp.Prelude as M

shouldReturn :: (HasCallStack, Show a, Eq a, MonadUnliftIO m) => m a -> a -> m ()
shouldReturn action expected =
  withRunInIO \runInIO -> do
    runInIO action `Syd.shouldReturn` expected
