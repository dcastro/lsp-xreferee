module Xreferee.Lsp.Util where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Control.Monad.State (StateT, get, put, runStateT)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.IxSet.Typed ((@+), (@<=), (@=), (@>=))
import Data.IxSet.Typed qualified as Ix
import Data.IxSet.Typed.Util qualified as Ix
import Data.Map qualified as Map
import Data.Map.Strict qualified as SM
import Data.Set qualified as Set
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server (MonadLsp)
import System.FilePath qualified as FP
import XReferee.SearchResult (Anchor, Reference)
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Git qualified as Git
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Types (ColumnEnd (..), ColumnStart (..), LineNum (..), SymbolEntry (..), SymbolIxsConstraint, SymbolLoc (..), SymbolSet, Symbols (..))
import Xreferee.Lsp.Types qualified as Types

deleteSymbolsForFileOrDirectory :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => Uri -> AppState -> m AppState
deleteSymbolsForFileOrDirectory dirUri appState = do
  let (anchors', deletedAnchorsUris) = delete @Anchor appState.symbols.anchors
      (references', deletedReferencesUris) = delete @Reference appState.symbols.references
      deletedUris = deletedAnchorsUris <> deletedReferencesUris

  forM_ deletedAnchorsUris \deletedUri -> do
    Log.debug $ "Deleted anchors from file: " <> tshow deletedUri
  forM_ deletedReferencesUris \deletedUri -> do
    Log.debug $ "Deleted references from file: " <> tshow deletedUri

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

loadSymbolsForFile :: Uri -> LByteString -> Int32 -> AppState -> AppState
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

    parseFile :: LByteString -> Uri -> Symbols
    parseFile contents uri =
      (LBS.lines contents `zip` [0 ..])
        <&> ( \(line, lineNum) ->
                let (anchors, refs) = X.parseLabels X.defaultDelims line
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

-- | Checks whether we should ignore or process a given file.
--
-- We ignore the `.git` folder, files ignored by git, and files outside the workspace.
--
-- #(ref:shouldHandleFile)
shouldHandleFile :: Uri -> AppM Bool
shouldHandleFile uri = do
  modifyStateWithoutDiagnostics \appState -> do
    (should, appState) <- flip runStateT appState $ shouldHandleFile' uri
    pure (appState, should)

-- NOTE: we can't have `(MonadState s m, MonadLsp c m)` because `StateT` does not and cannot implement `MonadLsp`.
-- `MonadLsp` implies `MonadUnliftIO`, and `MonadUnliftIO`, by definition, does not support stateful monads like `StateT`.
shouldHandleFile' :: (MonadReader r m, HasAppEnv r, MonadLsp config m) => Uri -> StateT AppState m Bool
shouldHandleFile' uri = do
  workspaceDir <- view workspaceDir
  appState0 <- get
  -- Check if we have this result cached from a previous check.
  case SM.lookup uri appState0.shouldHandleFiles of
    Just should -> pure should
    Nothing -> do
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
                      -- If the file is ignored by git, don't handle it.
                      liftIO $ not <$> Git.checkIgnore fp

      put $ appState0 {shouldHandleFiles = SM.insert uri should appState0.shouldHandleFiles}

      when (not should) do
        lift $ Log.debug $ "Ignoring file: " <> tshow uri

      pure should
