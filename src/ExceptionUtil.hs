{-# LANGUAGE CPP               #-}
{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples     #-}
{-# LANGUAGE FieldSelectors    #-}

-- | Exception utilities
--
-- Intended for qualified import.
--
-- > import ExceptionUtil qualified
--
-- Copied from: https://gist.github.com/edsko/49cc535d712048f6cac532e8a02ea374
-- See: https://well-typed.com/blog/2026/05/lay-annotation-land/
module ExceptionUtil (
    -- * Throwing and catching
    discardExceptions
    -- * Rendering
  , displayFullException
  , uncaughtExceptionHandler
  ) where

import Control.Exception
import Control.Exception.Annotation
import Control.Exception.Backtrace
import Control.Exception.Context
import Data.Foldable (toList)
import Data.Semigroup
import Data.String
import Data.Typeable
import Foreign.C
import GHC.IO
import GHC.Prim (State#, RealWorld)
import GHC.PrimopWrappers qualified as PrimOp
import System.IO
import Prelude

{-------------------------------------------------------------------------------
  Throwing and catching
-------------------------------------------------------------------------------}

discardExceptions :: IO () -> IO ()
discardExceptions (IO io) = IO $ PrimOp.catch# io handler
  where
    handler :: SomeException -> State# RealWorld -> (# State# RealWorld, () #)
    handler _se w = (# w, () #)

{-------------------------------------------------------------------------------
  Internal auxiliary: barebones rendering abstraction for nested indentation
-------------------------------------------------------------------------------}

data Doc = FromString String | MConcat [Doc] | Indent Int Doc

instance IsString  Doc where fromString = FromString
instance Monoid    Doc where mconcat    = MConcat
instance Semigroup Doc where sconcat    = MConcat . toList

fromLines :: String -> Doc
fromLines = mconcat . map fromString . lines

renderDoc :: Doc -> String
renderDoc = \d ->
      unlines
    $ map (\(i, str) -> replicate i ' ' ++ str)
    $ go [(0, d)]
  where
    go :: [(Int, Doc)] -> [(Int, String)]
    go []            = []
    go ((i, d) : ds) =
        case d of
          FromString str   -> (i, str) : go ds
          MConcat    ds'   -> go $ map (i,) ds' ++ ds
          Indent     i' d' -> go $ (i + i', d') : ds

{-------------------------------------------------------------------------------
  Rendering

  displayExceptionWithInfo
    9.10: does not exist in 9.10
    9.12: introduced and used in the default excption handler, but not
          exported from base (ghc-internal)
          WhileHandling calls displayException, not ..withInfo
-------------------------------------------------------------------------------}

data Classified = Classified{
      other         :: [SomeExceptionAnnotation]
    , backtraces    :: Maybe Backtraces
#if MIN_VERSION_base(4,21,0)
    , whileHandling :: Maybe WhileHandling
#endif
    }

classifyAnnotations :: [SomeExceptionAnnotation] -> Classified
classifyAnnotations =
    go $ Classified{
        other         = []
      , backtraces    = Nothing
#if MIN_VERSION_base(4,21,0)
      , whileHandling = Nothing
#endif
      }
  where
    go :: Classified -> [SomeExceptionAnnotation] -> Classified
    go acc [] = acc{other = reverse (other acc)}
    go acc (sa@(SomeExceptionAnnotation a):as)
      | Just a' <- cast a, Nothing <- backtraces acc
      = go acc{backtraces = Just a'} as

#if MIN_VERSION_base(4,21,0)
      | Just a' <- cast a, Nothing <- whileHandling acc
      = go acc{whileHandling = Just a'} as
#endif

      | otherwise
      = go acc{other = sa:other acc} as

-- | Display the full exception, including all annotations
--
-- Indentation is used to indicate exception nesting. 'Backtraces' and
-- 'WhileHandling' are shown last (for each exception), in that order.
displayFullException :: SomeException -> String
displayFullException =
    renderDoc . goException
  where
    goException :: SomeException -> Doc
    goException (SomeException e) = mconcat [
          fromString $ show (typeOf e)
        , Indent 2 $ mconcat [
              fromLines $ displayException e
            , foldMap goOther         $ other         classified
            , foldMap goBacktraces    $ backtraces    classified
#if MIN_VERSION_base(4,21,0)
            , foldMap goWhileHandling $ whileHandling classified
#endif
            ]
        ]
      where
        annotations :: [SomeExceptionAnnotation]
        ExceptionContext annotations = ?exceptionContext

        classified :: Classified
        classified = classifyAnnotations annotations

    goOther :: SomeExceptionAnnotation -> Doc
    goOther (SomeExceptionAnnotation ann) =
        fromLines $ displayExceptionAnnotation ann

    goBacktraces :: Backtraces -> Doc
    goBacktraces = fromLines . displayBacktraces

#if MIN_VERSION_base(4,21,0)
    goWhileHandling :: WhileHandling -> Doc
    goWhileHandling (WhileHandling e) = mconcat [
          "WhileHandling"
        , Indent 2 $ goException e
        ]
#endif

{-------------------------------------------------------------------------------
  Uncaught exception handler

  Adapted from the definition in base, but using 'displayFullException'.
-------------------------------------------------------------------------------}

uncaughtExceptionHandler :: SomeException -> IO ()
uncaughtExceptionHandler se = do
   discardExceptions $ hFlush stdout

   let exMsg = displayFullException se
       msg   = "Uncaught exception of type " ++ exMsg
   withCString "%s" $ \cfmt ->
    withCString msg $ \cmsg ->
      errorBelch cfmt cmsg

foreign import ccall unsafe "HsBase.h errorBelch2"
   errorBelch :: CString -> CString -> IO ()
