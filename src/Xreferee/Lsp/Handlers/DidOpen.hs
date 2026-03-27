module Xreferee.Lsp.Handlers.DidOpen where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.Map.Strict qualified as SM
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log
import Xreferee.Lsp.SendDiagnostics (modifyState)
import Xreferee.Lsp.Util qualified as Util

-- | Handle `didOpen` notifications.
--
-- When a file is opened, it may not necessarily reflect the state of the file on disk.
-- There are at least 2 situations where this can happen:
-- 1. When a user has unsaved changes in the file and restarts the editor,
--    the LSP will receive a `didOpen` with the contents of the "dirty" in-memory buffer.
-- 2. When the user starts the editor and quickly starts typing into the in-memory buffer before
--    the LSP server has been loaded.
--
-- This handler checks if the in-memory buffer is in a "dirty" state,
-- and if so, it reparses the file and updates the symbols.
handleDidOpen :: AppLogger -> Handler AppM 'LSP.Method_TextDocumentDidOpen
handleDidOpen logger = \req -> do
  logNot logger req
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let fileVersion = req ^. LSP.params . LSP.textDocument . LSP.version
  let contents = req ^. LSP.params . LSP.textDocument . LSP.text

  modifyState logger \appState0 -> do
    if not (checkIsDirty uri fileVersion appState0)
      then
        pure appState0
      else do
        pure $ Util.loadSymbolsForFile uri contents fileVersion appState0
  where
    checkIsDirty :: Uri -> Int32 -> AppState -> Bool
    checkIsDirty uri fileVersion appState =
      let lastSeenVersion = SM.findWithDefault 1 uri appState.fileVersions
       in -- NOTE: versions are not strictly monotonic.
          -- If a file is changed on disk (e.g. with `echo "#(ref:test4)" >> file.md`), AND the file is not currently opened in vscode,
          -- the next time the user opens it, the version will be reset to 1.
          fileVersion /= lastSeenVersion
