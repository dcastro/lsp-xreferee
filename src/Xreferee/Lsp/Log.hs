module Xreferee.Lsp.Log where

import ClassyPrelude
import Colog.Core (Severity (..), WithSeverity (..), (<&))
import Control.Lens
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Server (MonadLsp)
import Text.Pretty.Simple (pShowNoColor)
import Xreferee.Lsp.AppM (HasAppEnv, logger)

logReq :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TRequestMessage a -> m ()
logReq msg = do
  logger <- view logger
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logNot :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TNotificationMessage a -> m ()
logNot msg = do
  logger <- view logger
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logReqP :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TRequestMessage a -> m ()
logReqP msg = do
  logger <- view logger
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

logNotP :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TNotificationMessage a -> m ()
logNotP msg = do
  logger <- view logger
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

debug :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => Text -> m ()
debug msg = do
  logger <- view logger
  logger <& msg `WithSeverity` Debug

debugP :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => (Show a) => Text -> a -> m ()
debugP label x = do
  logger <- view logger
  logger <& (label <> ": " <> LT.toStrict (pShowNoColor x)) `WithSeverity` Debug

info :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => Text -> m ()
info msg = do
  logger <- view logger
  logger <& msg `WithSeverity` Info

err :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => Text -> m ()
err msg = do
  logger <- view logger
  logger <& msg `WithSeverity` Error
