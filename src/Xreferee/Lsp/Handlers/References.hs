module Xreferee.Lsp.Handlers.References where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Util qualified as Util

handleReferences :: Handler AppM 'LSP.Method_TextDocumentReferences
handleReferences req responder = do
  Log.logReq req

  let reqUri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let reqPos = req ^. LSP.params . LSP.position

  conn <- view conn

  Db.findAnchorAtPosition conn reqUri reqPos >>= \case
    Nothing ->
      responder $ Right $ LSP.InR LSP.Null
    Just anchor -> do
      -- Find the corresponding references
      refs <- Db.findReferencesWithName conn anchor.name
      let locs = refs <&> \ref -> Util.symbolLocToLspLocation ref
      responder $ Right $ LSP.InL locs
