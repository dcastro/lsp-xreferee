module Xreferee.Lsp.Handlers where

import Data.Time.Clock.POSIX qualified as Time
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Server qualified as LSP
import Xreferee.Lsp.AppM (AppM)
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.SendDiagnostics (sendDiagnostics)

-- | Basic setup for all request handlers.
--
-- * Add a backtrace to exceptions
-- * Install an exception handle, send an error message to the LSP client
-- * Log time spent handling the request
-- * Send diagnostics to the client
setupReqHandler :: forall from (method :: LSP.Method from 'LSP.Request). LSP.Handler AppM method -> LSP.Handler AppM method
setupReqHandler handler msg responder = do
  let method = msg._method
  annotateStackStringIO ("Handling " <> show method) do
    flip withException exHandler do
      timed method do
        handler msg responder
        sendDiagnostics

setupNotHandler :: forall from (method :: LSP.Method from 'LSP.Notification). LSP.Handler AppM method -> LSP.Handler AppM method
setupNotHandler handler msg = do
  let method = msg._method
  annotateStackStringIO ("Handling " <> show method) do
    flip withException exHandler do
      timed method do
        handler msg
        sendDiagnostics

-- Send a message to the client, but don't recover - let the LSP crash.
exHandler :: SomeException -> AppM ()
exHandler ex = do
  Log.err ("xreferee failed:\n" <> pack (displayFullException ex))

timed :: LSP.SMethod method -> AppM a -> AppM a
timed method action = do
  t0 <- liftIO Time.getPOSIXTime
  result <- action
  t1 <- liftIO Time.getPOSIXTime
  let duration = t1 - t0
  Log.debugP ("Handled " <> tshow method <> " in") duration
  pure result
