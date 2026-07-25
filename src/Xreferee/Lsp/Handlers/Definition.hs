module Xreferee.Lsp.Handlers.Definition where

import Control.Lens hiding (Indexable, Iso)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

handleDefinition :: Handler AppM 'LSP.Method_TextDocumentDefinition
handleDefinition = \req responder -> do
  Log.logReq req

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri
  let reqPos = req ^. LSP.params ^. LSP.position

  Db.findReferenceAtPosition reqUri reqPos >>= \case
    Nothing ->
      responder $ Right $ LSP.InR (LSP.InR LSP.Null)
    Just ref -> do
      -- Find the corresponding anchor(s).
      -- Ideally there will be 1, but there can also be 0 (if the reference is broken) or more than 1 (if there are duplicate anchors).
      anchors <- Db.findAnchorsWithName ref.name
      -- Build links from the reference to the anchor(s)
      let links =
            anchors
              <&> \anchor ->
                let refRange = Symbols.symbolLocToLspRange ref
                    anchorRange = Symbols.symbolLocToLspRange anchor
                 in LSP.DefinitionLink
                      LSP.LocationLink
                        { _originSelectionRange = Just refRange,
                          _targetUri = anchor.uri,
                          _targetRange = anchorRange,
                          _targetSelectionRange = anchorRange
                        }
      responder $ Right $ LSP.InR (LSP.InL links)
