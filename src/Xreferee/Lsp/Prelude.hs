module Xreferee.Lsp.Prelude
  ( module M,
    whenJust,
  )
where

import ClassyPrelude as M hiding
  ( -- Name collision with lsp's Handler
    Handler,
    bracket,
    bracketOnError,
    bracketOnError_,
    bracket_,
    catch,
    catchAny,
    catchAnyDeep,
    catchDeep,
    catchIO,
    catchJust,
    catchSyncOrAsync,
    catches,
    catchesDeep,
    -- Prelude's `displayException` does not include the `ExceptionContext`.
    --
    -- GHC 9.14 comes with `displayExceptionWithInfo` which includes the `ExceptionContext`,
    -- but only for the top-level exception.
    -- E.g. `catch` will wrap the underlying exception in `WhileHandling` before rethrowing.
    -- The wrapped exception's `ExceptionContext` will not be shown by `displayExceptionWithInfo`.
    --
    -- Instead, we use edsko's `displayFullException`.
    --
    -- https://well-typed.com/blog/2026/05/lay-annotation-land/#catch
    -- https://well-typed.com/blog/2026/05/lay-annotation-land/#caution-displaying-exceptions
    -- https://gist.github.com/edsko/49cc535d712048f6cac532e8a02ea374
    displayException,
    finally,
    handle,
    handleAny,
    handleAnyDeep,
    handleDeep,
    handleIO,
    handleJust,
    handleSyncOrAsync,
    onException,
    -- `unliftio`'s `throwIO` does not capture the callstack.
    throwIO,
    try,
    tryAny,
    tryAnyDeep,
    tryDeep,
    tryIO,
    tryJust,
    trySyncOrAsync,
    withException,
  )
import Control.Exception as M
  ( ExceptionWithContext (..),
    catchNoPropagate,
    rethrowIO,
  )
import Control.Exception.Safe as M
  ( -- Unlike `unliftio`, `safe-exceptions`'s `throwIO` does capture the callstack.
    throwIO,
    try,
  )
import Control.Lens as M (view)
import Data.Function as M
  ( (&),
  )
import ExceptionUtil as M
  ( -- Display exception with `ExceptionContext`
    displayFullException,
  )
import Language.LSP.Protocol.Types as M
  ( Uri (..),
  )
import Xreferee.Lsp.Exception as M

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust = for_
