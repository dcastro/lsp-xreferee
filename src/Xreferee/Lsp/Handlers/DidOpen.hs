module Xreferee.Lsp.Handlers.DidOpen where

import Control.Exception.Safe qualified as Safe
import Control.Lens hiding (Indexable, Iso)
import Data.ByteString.Lazy qualified as LBS
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

-- | Handle `didOpen` notifications.
--
-- When a file is opened, it may not necessarily reflect the state of the file on disk.
-- There are at least 2 situations where this can happen:
-- 1. When a user has unsaved changes in the file and restarts the editor,
--    the LSP will receive a `didOpen` with the contents of the "dirty" in-memory buffer.
--    In this case, the event will say the file version is 1.
-- 2. When the user starts the editor and quickly starts typing into the in-memory buffer before
--    the LSP server has been loaded.
--    In this case, the event will say the file version is > 1.
--
-- This handler checks if the in-memory buffer is in a "dirty" state,
-- and if so, it reparses the file and updates the symbols.
handleDidOpen :: Handler AppM 'LSP.Method_TextDocumentDidOpen
handleDidOpen = \req -> do
  Log.logNot req
  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  let contents = req ^. LSP.params . LSP.textDocument . LSP.text . to fromStrict . to encodeUtf8

  whenJust (LSP.uriToFilePath uri) \path -> do
    isDirty <- liftIO $ checkIfBufferIsDirty path contents
    Log.debugP "Buffer is dirty" isDirty
    case isDirty of
      Just False -> pure ()
      -- If the buffer is "dirty", or we failed to check if it's dirty, we reparse the file and update the symbols.
      Just True; Nothing -> Symbols.loadSymbolsForFile uri contents
  where
    -- Check if the buffer is "dirty", i.e. if the editor has unsaved changes.
    checkIfBufferIsDirty :: FilePath -> LByteString -> IO (Maybe Bool)
    checkIfBufferIsDirty fp contents =
      (Just <$> differsFromFile fp contents) `Safe.catch` \(_ :: IOException) -> do
        pure Nothing

    -- Check if the contents of the buffer match the file on disk.
    -- This reads a chunk at a time, so it's constant space.
    -- And short-circuits when it finds a difference.
    -- The chunk comparison is done with `memcmp`, so it's fast.
    --
    -- This takes about 0.2s on a 30 MB file, with a clean buffer (i.e. the file was read to EOF)
    differsFromFile :: FilePath -> LByteString -> IO Bool
    differsFromFile fp contents =
      withBinaryFile fp ReadMode \h -> do
        onDisk <- LBS.hGetContents h
        -- NOTE: `hGetContents` is lazy IO, so we MUST force the comparison before leaving this scope.
        -- `withBinaryFile` will close the handle.
        evaluate (onDisk /= contents)
