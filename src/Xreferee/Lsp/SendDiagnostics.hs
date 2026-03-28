module Xreferee.Lsp.SendDiagnostics where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.IxSet.Typed qualified as Ix
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import UnliftIO qualified as Unlift
import Unsafe.Coerce qualified as Unsafe
import XReferee.SearchResult (Anchor, Reference)
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Types (SymbolEntry (..), SymbolLoc (..), Symbols (..))
import Xreferee.Lsp.Util qualified as Util

-- | Modify the app state, and then send diagnostics to the client if the symbols have changed.
modifyState :: AppLogger -> (AppState -> AppM AppState) -> AppM ()
modifyState logger act = do
  env <- ask
  Unlift.modifyMVar_ env.state \appState0 -> do
    appState1 <- act appState0

    -- If the symbols didn't change, then the diagnostics won't change either, so we can skip computing diagnostics.
    if appState0.symbols == appState1.symbols
      then pure appState1
      else sendDiagnostics logger appState1

-- | A label that is shown next to each warning/error.
diagnosticsSource :: Maybe Text
diagnosticsSource = Just "xreferee"

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: AppLogger -> AppState -> AppM AppState
sendDiagnostics _logger state = do
  let anchorsGrouped = state.symbols.anchors & Ix.groupBy' @Anchor :: Map Anchor (Set (SymbolEntry Anchor))
  let refsGrouped = state.symbols.references & Ix.groupBy' @Reference :: Map Reference (Set (SymbolEntry Reference))

  -- Map keys have role "nominal", so we can't coerce between Anchors and References.
  -- Even though they are both newtypes of `Text`, it's not safe to coerce a map of Anchors into a map of References because
  -- they may have different implementations of `Ord` and thus be ordered differently in the map.
  -- However, we do know that `Anchor` and `Reference` have the same `Ord` implementation,
  -- so we use `unsafeCoerce`.
  let unusedAnchors = Map.difference anchorsGrouped (Unsafe.unsafeCoerce refsGrouped)
  let brokenRefs = Map.difference refsGrouped (Unsafe.unsafeCoerce anchorsGrouped)
  let duplicateAnchors = Map.filter (\locs -> length locs > 1) anchorsGrouped

  let unusedAnchorsDiagnostics = do
        (anchor, entries) <- Map.toList unusedAnchors
        entry <- Set.toList entries
        pure
          ( entry.loc.uri,
            [ LSP.Diagnostic
                { _range = Util.symbolLocToLspRange entry.loc,
                  _severity = Just LSP.DiagnosticSeverity_Warning,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Unused anchor: '" <> X.toLabel anchor <> "'",
                  _tags = Just ([LSP.DiagnosticTag_Unnecessary]),
                  _relatedInformation = Nothing,
                  _data_ = Nothing
                }
            ]
          )

  let brokenRefsDiagnostics = do
        (ref, entries) <- Map.toList brokenRefs
        entry <- Set.toList entries
        pure
          ( entry.loc.uri,
            [ LSP.Diagnostic
                { _range = Util.symbolLocToLspRange entry.loc,
                  _severity = Just LSP.DiagnosticSeverity_Error,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Broken reference: '" <> X.toLabel ref <> "'",
                  _tags = Nothing,
                  _relatedInformation = Nothing,
                  _data_ = Nothing
                }
            ]
          )

  let duplicateAnchorsDiagnostics = do
        (anchor, entries) <- Map.toList duplicateAnchors
        guard (length entries > 1)
        entry <- Set.toList entries
        let otherEntries = Set.filter (/= entry) entries
        pure
          ( entry.loc.uri,
            [ LSP.Diagnostic
                { _range = Util.symbolLocToLspRange entry.loc,
                  _severity = Just LSP.DiagnosticSeverity_Error,
                  _code = Nothing,
                  _codeDescription = Nothing,
                  _source = diagnosticsSource,
                  _message = "Duplicate anchor: '" <> X.toLabel anchor <> "'",
                  _tags = Nothing,
                  _relatedInformation =
                    Just $
                      Set.toList otherEntries <&> \otherEntry ->
                        LSP.DiagnosticRelatedInformation
                          { _location = Util.symbolLocToLspLocation otherEntry.loc,
                            _message = "Duplicate definition."
                          },
                  _data_ = Nothing
                }
            ]
          )

  -- Publish all diagnostics
  let allDiagnosticsByFile = Map.fromListWith (<>) $ unusedAnchorsDiagnostics <> brokenRefsDiagnostics <> duplicateAnchorsDiagnostics
  forM_ (Map.toList allDiagnosticsByFile) $ \(uri, diagnostics) -> do
    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (partitionBySource diagnostics)

  -- Clear diagnostics for files that had diagnostics before but don't have any now.
  let filesWithDiagnosticsNow = Map.keysSet allDiagnosticsByFile
  let filesWithDiagnosticsBefore = state.filesWithDiagnostics
  forM_ (Set.difference filesWithDiagnosticsBefore filesWithDiagnosticsNow) \uri -> do
    publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (Map.singleton diagnosticsSource mempty)

  pure state {filesWithDiagnostics = filesWithDiagnosticsNow}
