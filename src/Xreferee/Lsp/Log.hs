module Xreferee.Lsp.Log where

import ClassyPrelude
import Colog.Core (Severity (..), WithSeverity (..), (<&))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Language.LSP.Protocol.Message qualified as LSP
import Text.Pretty.Simple (pShowNoColor)
import Xreferee.Lsp.AppM

logReq :: (Show (LSP.MessageParams a)) => LSP.TRequestMessage a -> AppM ()
logReq msg = do
  logger <- asks (.logger)
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logNot :: (Show (LSP.MessageParams a)) => LSP.TNotificationMessage a -> AppM ()
logNot msg = do
  logger <- asks (.logger)
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logReqP :: (Show (LSP.MessageParams a)) => LSP.TRequestMessage a -> AppM ()
logReqP msg = do
  logger <- asks (.logger)
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

logNotP :: (Show (LSP.MessageParams a)) => LSP.TNotificationMessage a -> AppM ()
logNotP msg = do
  logger <- asks (.logger)
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

debug :: Text -> AppM ()
debug msg = do
  logger <- asks (.logger)
  logger <& msg `WithSeverity` Debug

debugP :: (Show a) => Text -> a -> AppM ()
debugP label x = do
  logger <- asks (.logger)
  logger <& (label <> ": " <> LT.toStrict (pShowNoColor x)) `WithSeverity` Debug

info :: Text -> AppM ()
info msg = do
  logger <- asks (.logger)
  logger <& msg `WithSeverity` Info

err :: Text -> AppM ()
err msg = do
  logger <- asks (.logger)
  logger <& msg `WithSeverity` Error
