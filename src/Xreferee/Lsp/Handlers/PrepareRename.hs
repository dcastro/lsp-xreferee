module Xreferee.Lsp.Handlers.PrepareRename where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Types (SymbolEntry (..), Symbols (..))
import Xreferee.Lsp.Util qualified as Util

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_prepareRename
handlePrepareRename :: Handler AppM 'LSP.Method_TextDocumentPrepareRename
handlePrepareRename = \req responder -> do
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let pos = req ^. LSP.params . LSP.position

  state <- getState
  -- NOTE: looking up in the symbols table may not be the best solution at the moment (re-parsing the line may be faster).
  -- But after we move the symbols table to an `IxSet`, then a symbol lookup may indeed be better.
  let maybeMatch = case (Util.findSymbolAtPosition uri pos state.symbols.anchors, Util.findSymbolAtPosition uri pos state.symbols.references) of
        (Just anchorEntry, _) -> Just (X.toLabel anchorEntry.symbol, anchorEntry.loc)
        (_, Just refEntry) -> Just (X.toLabel refEntry.symbol, refEntry.loc)
        (Nothing, Nothing) -> Nothing

  case maybeMatch of
    Nothing -> responder $ Right $ LSP.InR LSP.Null
    Just (symbol, loc) ->
      responder $
        Right $
          LSP.InL $
            LSP.PrepareRenameResult $
              LSP.InR $
                LSP.InL $
                  LSP.PrepareRenamePlaceholder
                    { _range = Util.symbolLocToLspRange loc,
                      _placeholder = symbol
                    }
