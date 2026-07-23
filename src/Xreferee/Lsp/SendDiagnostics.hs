module Xreferee.Lsp.SendDiagnostics where

import Control.Lens hiding (Indexable, Iso)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.LSP.Diagnostics
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Util qualified as Util

-- | A label that is shown next to each warning/error.
diagnosticsSource :: Maybe Text
diagnosticsSource = Just "xreferee"

-- | Analyze the file and send any diagnostics to the client in a
-- "textDocument/publishDiagnostics" notification
sendDiagnostics :: AppM ()
sendDiagnostics = do
  conn <- view conn
  appState <- getState
  -- If the symbols didn't change, then the diagnostics won't change either, so we can skip computing diagnostics.
  if not appState.isDbDirty
    then do
      Log.debug "sendDiagnostics: skip"
      pure ()
    else do
      Log.debug "sendDiagnostics: computing"
      unusedAnchors <- Db.findUnusedAnchors conn
      brokenRefs <- Db.findBrokenReferences conn
      duplicateAnchors <- Db.findDuplicateAnchors conn

      let unusedAnchorsDiagnostics = do
            anchor <- unusedAnchors
            -- entry <- Set.toList entries
            pure
              ( anchor.uri,
                [ LSP.Diagnostic
                    { _range = Util.symbolLocToLspRange anchor,
                      _severity = Just LSP.DiagnosticSeverity_Warning,
                      _code = Nothing,
                      _codeDescription = Nothing,
                      _source = diagnosticsSource,
                      _message = "Unused anchor: '" <> anchor.name <> "'",
                      _tags = Just [LSP.DiagnosticTag_Unnecessary],
                      _relatedInformation = Nothing,
                      _data_ = Nothing
                    }
                ]
              )

      let brokenRefsDiagnostics = do
            ref <- brokenRefs
            pure
              ( ref.uri,
                [ LSP.Diagnostic
                    { _range = Util.symbolLocToLspRange ref,
                      _severity = Just LSP.DiagnosticSeverity_Error,
                      _code = Nothing,
                      _codeDescription = Nothing,
                      _source = diagnosticsSource,
                      _message = "Broken reference: '" <> ref.name <> "'",
                      _tags = Nothing,
                      _relatedInformation = Nothing,
                      _data_ = Nothing
                    }
                ]
              )

      let duplicateAnchorsDiagnostics = do
            -- This `List.groupBy` relies on the db query returning anchors sorted by name
            -- @(ref:duplicate-anchors-sorted)
            anchors <- List.groupBy (\a b -> a.name == b.name) duplicateAnchors
            anchor <- anchors
            let otherAnchors = filter (/= anchor) anchors
            pure
              ( anchor.uri,
                [ LSP.Diagnostic
                    { _range = Util.symbolLocToLspRange anchor,
                      _severity = Just LSP.DiagnosticSeverity_Error,
                      _code = Nothing,
                      _codeDescription = Nothing,
                      _source = diagnosticsSource,
                      _message = "Duplicate anchor: '" <> anchor.name <> "'",
                      _tags = Nothing,
                      _relatedInformation =
                        Just
                          $ otherAnchors
                          <&> \otherAnchor ->
                            LSP.DiagnosticRelatedInformation
                              { _location = Util.symbolLocToLspLocation otherAnchor,
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
      let filesWithDiagnosticsBefore = appState.filesWithDiagnostics
      forM_ (Set.difference filesWithDiagnosticsBefore filesWithDiagnosticsNow) \uri -> do
        publishDiagnostics 100 (LSP.toNormalizedUri uri) Nothing (Map.singleton diagnosticsSource mempty)

      putState
        appState
          { filesWithDiagnostics = filesWithDiagnosticsNow,
            isDbDirty = False
          }
