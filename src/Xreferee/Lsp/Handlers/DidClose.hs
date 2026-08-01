module Xreferee.Lsp.Handlers.DidClose where

import Control.Lens hiding (Indexable, Iso)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import System.Directory qualified as Dir
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude

-- | Handle `didCLose` notifications.
handleDidClose :: Handler AppM 'LSP.Method_TextDocumentDidClose
handleDidClose = \req -> do
  Log.logNot req
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri

  whenJust (LSP.uriToFilePath uri) \path -> do
    liftIO (Dir.doesFileExist path) >>= \case
      True -> do
        -- If the file still exists on disk, do nothing.
        pure ()
      False -> do
        -- If the file no longer exists on disk, (because e.g. the user deleted it from disk and _then_ closed the editor tab),
        -- remove the symbols for that file.
        Db.deleteSymbolsForFile uri
