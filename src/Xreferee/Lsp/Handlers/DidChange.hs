module Xreferee.Lsp.Handlers.DidChange where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.Map.Strict qualified as SM
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Mixed.Rope qualified as Rope
import Database.SQLite.Simple (Connection)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (UInt, Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Language.LSP.VFS qualified as VFS
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Symbols qualified as Symbols

handleDidChange :: Handler AppM 'LSP.Method_TextDocumentDidChange
handleDidChange = \req -> do
  Log.logNot req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  vf <- Maybe.fromJust <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)
  let rope = vf ^. VFS.file_text

  let diffs = req ^. LSP.params . LSP.contentChanges

  conn <- view conn
  ApplyChangesResult linesToParse <- applyChanges conn uri diffs

  forM_ linesToParse \lineNum -> do
    let line =
          rope
            & Rope.getLine (fromIntegral @UInt @Word lineNum)
            & Rope.toText
            & LT.fromStrict
            & encodeUtf8
    let (anchors, refs) = X.parseLabels X.defaultDelims line

    forM_ anchors \(anchor, columnRange) -> do
      let symbol = Symbols.mkSymbol anchor uri (Db.LineNum lineNum) columnRange
      Db.insertAnchor conn symbol

    forM_ refs \(ref, columnRange) -> do
      let symbol = Symbols.mkSymbol ref uri (Db.LineNum lineNum) columnRange
      Db.insertReference conn symbol

  -- Update the version we have for this file.
  modifyState2 \appState ->
    appState {fileVersions = SM.insert uri (vf ^. VFS.lsp_version) appState.fileVersions}

-- | Calculates which lines we'll need to reparse after applying the given diffs.
-- Removes anchors/refs that are on lines that were modified by the diffs,
-- and updates the line numbers of anchors/refs that are located after the diffs.
applyChanges :: Connection -> Uri -> [LSP.TextDocumentContentChangeEvent] -> AppM ApplyChangesResult
applyChanges conn uri diffs =
  let initialState = ApplyChangesResult {linesToParse = []}
   in foldM go initialState diffs
  where
    go :: ApplyChangesResult -> LSP.TextDocumentContentChangeEvent -> AppM ApplyChangesResult
    go result diff =
      case diff of
        LSP.TextDocumentContentChangeEvent (LSP.InR _wholeDoc) -> error "We should only get partial document updates, not whole document updates"
        LSP.TextDocumentContentChangeEvent (LSP.InL diff) -> do
          let oldLineStart = diff ^. LSP.range . LSP.start . LSP.line
              oldLineEnd = diff ^. LSP.range . LSP.end . LSP.line
              oldLineCount = oldLineEnd - oldLineStart + 1
              newLineCount = fromIntegral @Int @UInt $ diff ^. LSP.text . to (T.count "\n") + 1

              -- How many lines were added (or removed) by this diff.
              lineDelta = newLineCount - oldLineCount

              -- Update the line numbers we need to reparse.
              -- If they occur after this diff, they need to be shifted by the line delta, just like the anchors/refs.
              linesToParse0 = result.linesToParse <&> (\lineNum -> if lineNum > oldLineEnd then lineNum + lineDelta else lineNum)

              -- We'll need to reparse all the lines that were modified by this diff.
              -- NOTE: we don't parse them _straight_ away, because the VFS only has the state of the file after all the diffs have been applied,
              -- so we need to wait until the end of the function to parse them, once we've processed all the diffs and updated our state accordingly.
              linesToParse1 = linesToParse0 <> [oldLineStart .. oldLineStart + newLineCount - 1]

          -- Discard anchors/refs on lines that were modified by this diff
          Db.deleteSymbolsInLineRange conn uri (Db.LineNum oldLineStart) (Db.LineNum oldLineEnd)

          -- Update the line numbers of anchors/refs that are after the diff
          Db.shiftSymbolsAfterLine conn uri (Db.LineNum oldLineEnd) lineDelta

          pure $
            result
              { linesToParse = linesToParse1
              }

data ApplyChangesResult = ApplyChangesResult
  { linesToParse :: [UInt]
  }
