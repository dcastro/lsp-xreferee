module Xreferee.Lsp.Handlers.PrepareRename where

import Control.Lens hiding (Indexable, Iso)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareRename
handlePrepareRename :: Handler AppM 'LSP.Method_TextDocumentPrepareRename
handlePrepareRename req responder = do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let pos = req ^. LSP.params . LSP.position

  maybeMatch <-
    Db.findAnchorAtPosition uri pos >>= \case
      Just symbol -> pure (Just symbol)
      Nothing -> Db.findReferenceAtPosition uri pos

  case maybeMatch of
    Nothing -> responder $ Right $ LSP.InR LSP.Null
    Just symbol ->
      responder $
        Right $
          LSP.InL $
            LSP.PrepareRenameResult $
              LSP.InR $
                LSP.InL $
                  LSP.PrepareRenamePlaceholder
                    { _range = Symbols.symbolLocToLspRange symbol,
                      _placeholder = symbol.name
                    }
