module Xreferee.Lsp.Log where

import Colog.Core (Severity (..), WithSeverity (..), (<&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Language.LSP.Protocol.Message qualified as LSP
import Text.Pretty.Simple (pShowNoColor)
import Xreferee.Lsp.AppM

logReq :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TRequestMessage a -> AppM ()
logReq logger msg = do
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logNot :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TNotificationMessage a -> AppM ()
logNot logger msg = do
  logger <& (T.pack $ show msg) `WithSeverity` Debug

logReqP :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TRequestMessage a -> AppM ()
logReqP logger msg = do
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

logNotP :: (Show (LSP.MessageParams a)) => AppLogger -> LSP.TNotificationMessage a -> AppM ()
logNotP logger msg = do
  logger <& (LT.toStrict $ pShowNoColor msg) `WithSeverity` Debug

debug :: AppLogger -> Text -> AppM ()
debug logger msg = do
  logger <& msg `WithSeverity` Debug

debugP :: (Show a) => AppLogger -> a -> AppM ()
debugP logger x = do
  logger <& (LT.toStrict $ pShowNoColor x) `WithSeverity` Debug
