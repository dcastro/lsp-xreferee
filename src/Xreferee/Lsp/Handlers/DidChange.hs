module Xreferee.Lsp.Handlers.DidChange where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.IxSet.Typed ((@=), (@>), (@>=<=))
import Data.IxSet.Typed qualified as Ix
import Data.Map.Strict qualified as SM
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Mixed.Rope qualified as Rope
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (UInt, Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Language.LSP.VFS qualified as VFS
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log
import Xreferee.Lsp.SendDiagnostics (modifyState)
import Xreferee.Lsp.Types (LineNum (..), SymbolEntry (..), SymbolIxsConstraint, SymbolLoc (..), SymbolSet, Symbols (..))
import Xreferee.Lsp.Types qualified as Types

handleDidChange :: Handler AppM 'LSP.Method_TextDocumentDidChange
handleDidChange = \req -> do
  logNot req

  let uri = req ^. LSP.params . LSP.textDocument . LSP.uri
  vf <- Maybe.fromJust <$> LSP.getVirtualFile (LSP.toNormalizedUri uri)
  let rope = vf ^. VFS.file_text

  let diffs = req ^. LSP.params . LSP.contentChanges

  modifyState \appState0 -> do
    -- Apply the diffs to our symbols tables
    ApplyChangesResult linesToParse symbols1 <- applyChanges appState0.symbols uri diffs

    -- Parse any new symbols introduces by the diffs
    let newSymbols =
          linesToParse
            <&> ( \lineNum ->
                    let line = rope & Rope.getLine (fromIntegral @UInt @Word lineNum) & Rope.toText
                        (anchors, refs) = X.parseLabels (LT.fromStrict line) 1 -- 1-based columns
                        mkSymbolEntry :: forall symbol. symbol -> X.ColumnRange -> SymbolEntry symbol
                        mkSymbolEntry sym columnRange =
                          SymbolEntry
                            { symbol = sym,
                              loc =
                                SymbolLoc
                                  { uri,
                                    lineNum,
                                    columnRange = Types.mkColumnRange columnRange
                                  }
                            }
                     in Symbols
                          { anchors = anchors <&> uncurry mkSymbolEntry & Ix.fromList,
                            references = refs <&> uncurry mkSymbolEntry & Ix.fromList
                          }
                )
            & mconcat

    pure
      appState0
        { symbols = symbols1 <> newSymbols,
          -- Update the version we have for this file.
          fileVersions = SM.insert uri (vf ^. VFS.lsp_version) appState0.fileVersions
        }

-- | Calculates which lines we'll need to reparse after applying the given diffs.
-- Removes anchors/refs that are on lines that were modified by the diffs,
-- and updates the line numbers of anchors/refs that are located after the diffs.
applyChanges :: Symbols -> Uri -> [LSP.TextDocumentContentChangeEvent] -> AppM ApplyChangesResult
applyChanges symbols uri diffs =
  let initialState = ApplyChangesResult {linesToParse = [], symbols = symbols}
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

              -- Get the symbols defined in this file
              thisFileAnchors = result.symbols.anchors @= uri
              thisFileReferences = result.symbols.references @= uri

              -- Discard anchors/refs on lines that were modified by this diff
              deleteEntriesInDiff :: forall symbol. (SymbolIxsConstraint symbol) => SymbolSet symbol -> SymbolSet symbol -> SymbolSet symbol
              deleteEntriesInDiff thisFileSymbols allSymbols =
                let entriesToDelete = thisFileSymbols @>=<= (LineNum oldLineStart, LineNum oldLineEnd)
                 in Ix.difference allSymbols entriesToDelete

              -- Update the line numbers of anchors/refs that are after the diff
              shiftEntriesAfterDiff :: forall symbol. (SymbolIxsConstraint symbol) => SymbolSet symbol -> SymbolSet symbol -> SymbolSet symbol
              shiftEntriesAfterDiff thisFileSymbols allSymbols =
                let entriesToShift = thisFileSymbols @> LineNum oldLineEnd
                    shiftedEntries =
                      entriesToShift
                        & Ix.toList
                        <&> (\entry -> entry {loc = entry.loc {lineNum = entry.loc.lineNum + lineDelta}})
                        & Ix.fromList
                 in Ix.difference allSymbols entriesToShift
                      & Ix.union shiftedEntries

              -- Update the line numbers we need to reparse.
              -- If they occur after this diff, they need to be shifted by the line delta, just like the anchors/refs.
              linesToParse0 = result.linesToParse <&> (\lineNum -> if lineNum > oldLineEnd then lineNum + lineDelta else lineNum)

              -- We'll need to reparse all the lines that were modified by this diff.
              -- NOTE: we don't parse them _straight_ away, because the VFS only has the state of the file after all the diffs have been applied,
              -- so we need to wait until the end of the function to parse them, once we've processed all the diffs and updated our state accordingly.
              linesToParse1 = linesToParse0 <> [oldLineStart .. oldLineStart + newLineCount - 1]

          let anchors0 =
                result.symbols.anchors
                  & deleteEntriesInDiff thisFileAnchors
                  & shiftEntriesAfterDiff thisFileAnchors
          let refs0 =
                result.symbols.references
                  & deleteEntriesInDiff thisFileReferences
                  & shiftEntriesAfterDiff thisFileReferences

          pure $
            result
              { linesToParse = linesToParse1,
                symbols =
                  result.symbols
                    { anchors = anchors0,
                      references = refs0
                    }
              }

data ApplyChangesResult = ApplyChangesResult
  { linesToParse :: [UInt],
    symbols :: Symbols
  }
