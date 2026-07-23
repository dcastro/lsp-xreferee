module Xreferee.Lsp.Handlers.Rename where

import Control.Lens hiding (Indexable, Iso)
import Data.Map qualified as Map
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Util qualified as Util

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename
handleRename :: Handler AppM 'LSP.Method_TextDocumentRename
handleRename req responder = do
  Log.logReq req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let pos = req ^. LSP.params . LSP.position
  let newLabelName = req ^. LSP.params . LSP.newName

  conn <- view conn
  maybeMatch <-
    Db.findAnchorAtPosition conn uri pos >>= \case
      Just symbol -> pure (Just symbol)
      Nothing -> Db.findReferenceAtPosition conn uri pos

  case maybeMatch of
    Nothing -> responder $ Right $ LSP.InR LSP.Null
    Just symbol -> do
      matchingAnchors <- Db.findAnchorsWithName conn symbol.name
      let anchorEdits :: Map Uri [LSP.TextEdit] =
            matchingAnchors
              <&> ( \anchor ->
                      ( anchor.uri,
                        [ LSP.TextEdit
                            { _range = Util.symbolLocToLspRange anchor,
                              _newText = newLabelName & X.Anchor & X.renderLabel X.defaultDelims
                            }
                        ]
                      )
                  )
              & Map.fromListWith (<>)

      matchingRefs <- Db.findReferencesWithName conn symbol.name
      let refEdits :: Map Uri [LSP.TextEdit] =
            matchingRefs
              <&> ( \ref ->
                      ( ref.uri,
                        [ LSP.TextEdit
                            { _range = Util.symbolLocToLspRange ref,
                              _newText = newLabelName & X.Reference & X.renderLabel X.defaultDelims
                            }
                        ]
                      )
                  )
              & Map.fromListWith (<>)

      let workspaceEdit =
            LSP.WorkspaceEdit
              { _changes = Just $ Map.unionWith (<>) anchorEdits refEdits,
                _documentChanges = Nothing,
                _changeAnnotations = Nothing
              }

      responder (Right $ LSP.InL workspaceEdit)
