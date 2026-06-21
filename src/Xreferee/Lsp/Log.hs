module Xreferee.Lsp.Log where

import ClassyPrelude
import Colog.Core (Severity (..), WithSeverity (..), (<&))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Server (MonadLsp)
import Text.Pretty.Simple (pShowNoColor)
import Xreferee.Lsp.AppM (AppEnv, getLogger)

logReq :: (MonadReader AppEnv m, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TRequestMessage a -> m ()
logReq msg = do
  logger <- asks getLogger
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logNot :: (MonadReader AppEnv m, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TNotificationMessage a -> m ()
logNot msg = do
  logger <- asks getLogger
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logReqP :: (MonadReader AppEnv m, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TRequestMessage a -> m ()
logReqP msg = do
  logger <- asks getLogger
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

logNotP :: (MonadReader AppEnv m, MonadLsp config m) => (Show (LSP.MessageParams a)) => LSP.TNotificationMessage a -> m ()
logNotP msg = do
  logger <- asks getLogger
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

debug :: (MonadReader AppEnv m, MonadLsp config m) => Text -> m ()
debug msg = do
  logger <- asks getLogger
  logger <& msg `WithSeverity` Debug

debugP :: (MonadReader AppEnv m, MonadLsp config m) => (Show a) => Text -> a -> m ()
debugP label x = do
  logger <- asks getLogger
  logger <& (label <> ": " <> LT.toStrict (pShowNoColor x)) `WithSeverity` Debug

info :: (MonadReader AppEnv m, MonadLsp config m) => Text -> m ()
info msg = do
  logger <- asks getLogger
  logger <& msg `WithSeverity` Info

err :: (MonadReader AppEnv m, MonadLsp config m) => Text -> m ()
err msg = do
  logger <- asks getLogger
  logger <& msg `WithSeverity` Error
