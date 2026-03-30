module Xreferee.Lsp.Handlers.Rename where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.IxSet.Typed ((@=))
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Types (SymbolEntry (..), Symbols (..))
import Xreferee.Lsp.Util qualified as Util

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
handleRename :: Handler AppM 'LSP.Method_TextDocumentRename
handleRename = \req responder -> do
  Log.logReq req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let pos = req ^. LSP.params . LSP.position
  let newLabelName = req ^. LSP.params . LSP.newName

  state <- getState

  let maybeMatch = case (Util.findSymbolAtPosition uri pos state.symbols.anchors, Util.findSymbolAtPosition uri pos state.symbols.references) of
        (Just anchorEntry, _) -> Just $ X.toLabel anchorEntry.symbol
        (_, Just refEntry) -> Just $ X.toLabel refEntry.symbol
        (Nothing, Nothing) -> Nothing

  case maybeMatch of
    Nothing -> responder $ Right $ LSP.InR LSP.Null
    Just label -> do
      let anchor = label & X.fromLabel @X.Anchor
      let ref = label & X.fromLabel @X.Reference

      let anchorEdits :: Map Uri [LSP.TextEdit] =
            state.symbols.anchors
              @= anchor
              & Ix.groupBy' @Uri
              <&> \entries ->
                Set.toList entries
                  <&> \entry ->
                    LSP.TextEdit
                      { _range = Util.symbolLocToLspRange entry.loc,
                        _newText = newLabelName & X.fromLabel @X.Anchor & X.renderLabel
                      }

      let refEdits :: Map Uri [LSP.TextEdit] =
            state.symbols.references
              @= ref
              & Ix.groupBy' @Uri
              <&> \entries ->
                Set.toList entries
                  <&> \entry ->
                    LSP.TextEdit
                      { _range = Util.symbolLocToLspRange entry.loc,
                        _newText = newLabelName & X.fromLabel @X.Reference & X.renderLabel
                      }

      let workspaceEdit =
            LSP.WorkspaceEdit
              { _changes = Just $ Map.unionWith (<>) anchorEdits refEdits,
                _documentChanges = Nothing,
                _changeAnnotations = Nothing
              }

      responder (Right $ LSP.InL workspaceEdit)
