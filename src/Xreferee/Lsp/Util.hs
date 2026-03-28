module Xreferee.Lsp.Util where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.IxSet.Typed ((@+), (@<=), (@=), (@>=))
import Data.IxSet.Typed qualified as Ix
import Data.IxSet.Typed.Util qualified as Ix
import Data.Map qualified as Map
import Data.Map.Strict qualified as SM
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as LSP
import System.Exit (ExitCode (..))
import System.FilePath qualified as FP
import System.Process qualified as P
import XReferee.SearchResult (Anchor, Reference)
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log
import Xreferee.Lsp.Types (ColumnEnd (..), ColumnStart (..), LineNum (..), SymbolEntry (..), SymbolIxsConstraint, SymbolLoc (..), SymbolSet, Symbols (..))
import Xreferee.Lsp.Types qualified as Types

deleteSymbolsForFileOrDirectory :: AppLogger -> Uri -> AppState -> AppM AppState
deleteSymbolsForFileOrDirectory logger dirUri appState = do
  let (anchors', deletedAnchorsUris) = delete @Anchor appState.symbols.anchors
      (references', deletedReferencesUris) = delete @Reference appState.symbols.references
      deletedUris = deletedAnchorsUris <> deletedReferencesUris

  forM_ deletedAnchorsUris \deletedUri -> do
    debug logger $ "Deleted anchors from file: " <> tshow deletedUri
  forM_ deletedReferencesUris \deletedUri -> do
    debug logger $ "Deleted references from file: " <> tshow deletedUri

  pure
    appState
      { symbols =
          appState.symbols
            { anchors = anchors',
              references = references'
            },
        fileVersions = SM.withoutKeys appState.fileVersions deletedUris
      }
  where
    delete :: (SymbolIxsConstraint symbol) => SymbolSet symbol -> (SymbolSet symbol, Set Uri)
    delete symbols =
      let allUris = Ix.groupBy' @Uri symbols & Map.keysSet
          urisToDelete = allUris & Set.filter (\u -> u `isWithinDir` dirUri)
          symbols' = Ix.deleteMany symbols (\entries -> entries @+ Set.toList urisToDelete)
       in (symbols', urisToDelete)

-- Checks if a URI points to a file within a given directory.
isWithinDir :: Uri -> Uri -> Bool
isWithinDir file dir =
  addTrailingPathSeparator dir.getUri `T.isPrefixOf` file.getUri
  where
    -- We MUST add a trailing path separator.
    -- Otherwise, `isWithinDir ./foobar/file.md ./foo` would incorrectly be `True`.
    addTrailingPathSeparator :: Text -> Text
    addTrailingPathSeparator =
      T.pack . FP.addTrailingPathSeparator . T.unpack

loadSymbolsForFile :: Uri -> Text -> Int32 -> AppState -> AppState
loadSymbolsForFile uri contents fileVersion appState0 =
  let -- Remove the symbols for this file
      appState1 = deleteSymbolsForFile uri appState0

      -- Parse the new symbols for this file
      newSymbols = parseFile contents uri
   in appState1
        { symbols = appState1.symbols <> newSymbols,
          -- Update the version we have for this file.
          fileVersions = SM.insert uri fileVersion appState1.fileVersions
        }
  where
    deleteSymbolsForFile :: Uri -> AppState -> AppState
    deleteSymbolsForFile uri appState =
      appState
        { symbols =
            appState.symbols
              { anchors = Ix.deleteMany appState.symbols.anchors (\anchors -> anchors @= uri),
                references = Ix.deleteMany appState.symbols.references (\references -> references @= uri)
              },
          fileVersions = SM.delete uri appState.fileVersions
        }

    parseFile :: Text -> Uri -> Symbols
    parseFile contents uri =
      (T.lines contents `zip` [0 ..])
        <&> ( \(line, lineNum) ->
                let (anchors, refs) = X.parseLabels (LT.fromStrict line) 1 -- 1-based columns
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

symbolLocToLspRange :: SymbolLoc -> LSP.Range
symbolLocToLspRange loc =
  LSP.Range
    { _start =
        LSP.Position
          { _line = loc.lineNum,
            _character = loc.columnRange.start
          },
      _end =
        LSP.Position
          { _line = loc.lineNum,
            _character = loc.columnRange.end + 1
          }
    }

symbolLocToLspLocation :: SymbolLoc -> LSP.Location
symbolLocToLspLocation loc =
  LSP.Location
    { _uri = loc.uri,
      _range = symbolLocToLspRange loc
    }

findSymbolAtPosition :: (SymbolIxsConstraint symbol) => Uri -> LSP.Position -> SymbolSet symbol -> Maybe (SymbolEntry symbol)
findSymbolAtPosition reqUri reqPos symbols =
  let reqLine = reqPos ^. LSP.line
      reqColumn = reqPos ^. LSP.character
   in Ix.getOne $
        symbols
          @= reqUri
          @= LineNum reqLine
          @<= ColumnStart reqColumn
          @>= ColumnEnd reqColumn

-- | Ignores the `.git` folder, files ignored by git, and files outside the workspace.
--
-- NOTE:
-- When the LSP server starts up, we use the `xreferee` package to search for symbols.
-- This uses `git grep`, which only looks for tracked files, not untracked files.
--
-- However, after the server starts up:
--   * while a developer is actively working on a project, they would also expect the LSP server to consider untracked files.
--   * even if we decided to only look at tracked files, we'd have to have a way of detecting in real-time when a file
--     becomes tracked (e.g. when a developer runs `git add`), so that we can start considering that file as well.
--     I'm not sure there's an efficient way of doing that.
--
-- For that reason, we're considering tracked AND untracked files, and only ignore files targeted by `.gitignore`.
shouldHandleFile :: AppLogger -> [FilePath] -> Uri -> AppM Bool
shouldHandleFile logger workspaceDir uri = do
  should <- case LSP.uriToFilePath uri of
    Nothing -> pure False
    Just fp ->
      let fp' = FP.splitDirectories fp
       in -- Ignore .git files
          if ".git" `elem` fp'
            then pure False
            else
              -- If the file is outside the workspace, ignore it.
              if not (workspaceDir `isPrefixOf` fp')
                then pure False
                else do
                  -- If the file is ignored by git, ignore it.
                  exitCode <- liftIO $ P.rawSystem "git" ["check-ignore", fp]
                  case exitCode of
                    ExitSuccess -> pure False -- The file is ignored by git, so we should ignore it too.
                    ExitFailure _ -> pure True -- The file is not ignored by git, so we should handle it.
  when (not should) do
    debugP logger "Ignoring file" uri
  pure should
