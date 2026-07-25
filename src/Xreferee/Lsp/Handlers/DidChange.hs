module Xreferee.Lsp.Handlers.DidChange where

import Control.Lens hiding (Indexable, Iso)
import Data.Ix (inRange)
import Data.Map.Strict qualified as SM
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Mixed.Rope qualified as Rope
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (UInt)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Language.LSP.VFS qualified as VFS
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

handleDidChange :: Handler AppM 'LSP.Method_TextDocumentDidChange
handleDidChange = \req -> do
  Log.logNot req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  vf <- Maybe.fromJust <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)
  let rope = vf ^. VFS.file_text

  let diffs = req ^. LSP.params . LSP.contentChanges

  ApplyChangesResult linesToParse <- applyChanges uri diffs

  -- Parse the lines that were modified by the diffs, and insert the new anchors/refs into the DB.
  let parseLine lineNum =
        rope
          & Rope.getLine (fromIntegral @UInt @Word lineNum)
          & Rope.toText
          & LT.fromStrict
          & encodeUtf8
          & Symbols.parseLine uri (Db.LineNum lineNum)
  let (anchors, refs) = foldMap parseLine linesToParse
  Db.insertAnchors anchors
  Db.insertReferences refs

  -- Update the version we have for this file.
  modifyState \appState ->
    appState {fileVersions = SM.insert uri (vf ^. VFS.lsp_version) appState.fileVersions}

-- | Calculates which lines we'll need to reparse after applying the given diffs.
-- Removes anchors/refs that are on lines that were modified by the diffs,
-- and updates the line numbers of anchors/refs that are located after the diffs.
applyChanges :: Uri -> [LSP.TextDocumentContentChangeEvent] -> AppM ApplyChangesResult
applyChanges uri diffs =
  let initialState = ApplyChangesResult {linesToParse = mempty}
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
              lineDelta :: Int = fromIntegral @UInt @Int newLineCount - fromIntegral @UInt @Int oldLineCount

              linesToParse0 =
                result.linesToParse
                  -- Lines inside this diff's range are stale (their content was consumed/rewritten by this diff,
                  -- and will be re-added below if still needed), so drop them, just like `deleteSymbolsInLineRange`
                  -- does for the symbols in the DB.
                  -- Lines after this diff need to be shifted by the line delta, just like the anchors/refs.
                  & Set.filter (not . inRange (oldLineStart, oldLineEnd))
                  -- Update the line numbers we need to reparse.
                  -- If they occur after this diff, they need to be shifted by the line delta,
                  -- just like `shiftSymbolsAfterLine` does for the symbols in the DB.
                  & Set.map \lineNum ->
                    if lineNum > oldLineEnd then lineNum `uintSum` lineDelta else lineNum

              -- We'll need to reparse all the lines that were modified by this diff.
              -- NOTE: we don't parse them _straight_ away, because the VFS only has the state of the file after all the diffs have been applied,
              -- so we need to wait until the end of the function to parse them, once we've processed all the diffs and updated our state accordingly.
              linesToParse1 = linesToParse0 <> Set.fromList [oldLineStart .. oldLineStart + newLineCount - 1]

          -- Discard anchors/refs on lines that were modified by this diff
          Db.deleteSymbolsInLineRange uri (Db.LineNum oldLineStart) (Db.LineNum oldLineEnd)

          -- Update the line numbers of anchors/refs that are after the diff
          Db.shiftSymbolsAfterLine uri (Db.LineNum oldLineEnd) lineDelta

          pure $
            result
              { linesToParse = linesToParse1
              }

uintSum :: UInt -> Int -> UInt
uintSum a b = fromIntegral @Int @UInt $ fromIntegral @UInt @Int a + b

data ApplyChangesResult = ApplyChangesResult
  { -- NOTE: this has to be a set, so we don't end up parsing the same line twice and, thus, end up with duplicate symbols.
    linesToParse :: Set UInt
  }
