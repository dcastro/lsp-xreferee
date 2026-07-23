module Xreferee.Lsp.Exception where

import ClassyPrelude
import Control.Exception qualified as Ex
import GHC.Stack.Annotation.Experimental qualified as Exp

-- | @'annotateStackStringIO' msg b@ annotates the evaluation stack of @b@
-- with the value @msg@.
--
-- When decoding the call stack, the annotation frames can be used to add more
-- information to stack traces.
--
-- Experimental, will be included in GHC 9.16
-- See: https://well-typed.com/blog/2025/09/better-haskell-stack-traces/
annotateStackStringIO :: forall b m. (MonadUnliftIO m) => String -> m b -> m b
annotateStackStringIO msg action = do
  withRunInIO \run -> Exp.annotateStackStringIO msg $ run action

-- | Similar to `withException` from `unliftio` and `safe-exception`, but
-- does not discard the annotations before rethrowing the exception.
--
-- See: https://well-typed.com/blog/2026/05/lay-annotation-land/#caution-rethrowing-the-same-exception
withException :: forall e a b m. (Exception e, MonadUnliftIO m) => m a -> (e -> m b) -> m a
withException action act = do
  withRunInIO \run ->
    withException' (run action) (run . act)
  where
    withException' :: forall e a b. (Exception e) => IO a -> (e -> IO b) -> IO a
    withException' action act =
      Ex.catchNoPropagate @e action \e@(Ex.ExceptionWithContext _ inner) -> do
        _ <- act inner
        Ex.rethrowIO e
