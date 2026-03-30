module Xreferee.Lsp.Handlers.Definition where

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
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Types (SymbolEntry (..), SymbolLoc (..), Symbols (..))
import Xreferee.Lsp.Types qualified as Types
import Xreferee.Lsp.Util qualified as Util

handleDefinition :: Handler AppM 'LSP.Method_TextDocumentDefinition
handleDefinition = \req responder -> do
  Log.logReq req

  let reqUri = req ^. LSP.params ^. LSP.textDocument ^. LSP.uri
  let reqPos = req ^. LSP.params ^. LSP.position

  state <- getState

  case Util.findSymbolAtPosition reqUri reqPos state.symbols.references of
    Nothing ->
      responder $ Right $ LSP.InR (LSP.InR LSP.Null)
    Just refEntry -> do
      -- Find the corresponding anchor(s).
      -- Ideally there will be 1, but there can also be 0 (if the reference is broken) or more than 1 (if there are duplicate anchors).
      let anchor = refEntry.symbol & X.toLabel & X.fromLabel @X.Anchor
      -- Build links from the reference to the anchor(s)
      let links =
            state.symbols.anchors
              @= anchor
              & Ix.toList
              <&> \anchorEntry ->
                let refRange =
                      LSP.Range
                        { _start = LSP.Position refEntry.loc.lineNum refEntry.loc.columnRange.start,
                          _end = LSP.Position refEntry.loc.lineNum (refEntry.loc.columnRange.end + 1)
                        }
                    anchorRange = Util.symbolLocToLspRange anchorEntry.loc
                 in LSP.DefinitionLink
                      LSP.LocationLink
                        { _originSelectionRange = Just refRange,
                          _targetUri = anchorEntry.loc.uri,
                          _targetRange = anchorRange,
                          _targetSelectionRange = anchorRange
                        }
      responder $ Right $ LSP.InR (LSP.InL links)
