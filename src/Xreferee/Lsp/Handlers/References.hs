module Xreferee.Lsp.Handlers.References where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.IxSet.Typed ((@=))
import Data.IxSet.Typed qualified as Ix
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log
import Xreferee.Lsp.Types (SymbolEntry (..), Symbols (..))
import Xreferee.Lsp.Util qualified as Util

handleReferences :: Handler AppM 'LSP.Method_TextDocumentReferences
handleReferences = \req responder -> do
  logReq req

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri
  let reqPos = req ^. LSP.params ^. LSP.position

  state <- getState

  case Util.findSymbolAtPosition reqUri reqPos state.symbols.anchors of
    Nothing ->
      responder $ Right $ LSP.InR LSP.Null
    Just anchorEntry -> do
      -- Find the corresponding references
      let ref = anchorEntry.symbol & X.toLabel & X.fromLabel @X.Reference
      let locs = state.symbols.references @= ref & Ix.toList <&> \refEntry -> Util.symbolLocToLspLocation refEntry.loc
      responder $ Right $ LSP.InL locs
