module Xreferee.Lsp.Handlers.DidChangeWatchedFiles where

import ClassyPrelude hiding (Handler)
import Control.Lens hiding (Indexable, Iso)
import Data.Maybe qualified as Maybe
import Data.Text.IO qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import UnliftIO qualified as Unlift
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Log
import Xreferee.Lsp.SendDiagnostics (sendDiagnostics)
import Xreferee.Lsp.Util qualified as Util

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
handleDidChangeWatchedFiles :: AppLogger -> Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles
handleDidChangeWatchedFiles logger = \req -> do
  logNot logger req
  appState0 <- getState

  let fileEvents = dedupFileCreatedEvents $ req ^. LSP.params . LSP.changes

  forM_ fileEvents \fileEvent -> do
    let uri = fileEvent ^. LSP.uri
    when (shouldHandleFile uri) $
      case fileEvent ^. LSP.type_ of
        LSP.FileChangeType_Changed -> do
          -- NOTE: when a file is changed on disk AND is open in the editor, either:
          --  * The user edited the file and saved the changes
          --      * in which case we don't need to handle it here
          --  * The file was changed on disk, and the editor buffer was updated as a result
          --      * e.g. the user switched git branches
          --      * in which case we also don't need to handle it here, because we'll receive a `didChange` notification with the new contents of the file.
          --  * The file was changed on disk, but the editor buffer was not updated
          --      * e.g. the user has unsaved changes in the editor and then switches branches
          --      * The file on disk and the editor buffer are now out of sync. We prioritize the buffer, so we don't need to handle this event.
          --
          -- In other words: we only care about this event if the file is NOT open in the editor.
          whenM (not <$> isFileOpen uri) do
            -- We'll get "changed" events for directories if e.g. the user sets attributes or changes permissions on the directory.
            -- We should ignore those events.
            whenM (isFileAndExists uri) do
              debug logger $ "Reloading file from disk: " <> tshow uri
              contents <- liftIO $ T.readFile (LSP.uriToFilePath uri & Maybe.fromJust)
              -- NOTE: If a file is changed on disk (e.g. with `echo "#(ref:test4)" >> file.md`), AND the file is not currently opened in vscode,
              -- the next time the user opens it, the version will be reset to 1.
              let fileVersion = 1
              modifyState $ pure . Util.loadSymbolsForFile uri contents fileVersion
        LSP.FileChangeType_Created -> do
          -- NOTE: this is triggered when:
          --  * a file is created via the editor (we receive a `didOpen` notification followed by a `didChangeWatchedFiles`).
          --  * a file is created outside the editor (we ONLY receive `didChangeWatchedFiles` notifications)
          --  * a file is renamed via the editor / outside the editor (we ONLY receive `didChangeWatchedFiles` notifications).
          --
          -- When a file is renamed and it's open in the editor, we'll only receive 2x `didChangeWatchedFiles` (deleted & created),
          -- We won't receive any other notifications.
          -- For this reason, we always have to handle this event here, without checking `isFileOpen`.
          --
          -- The downside is that when a file is created via the editor, we'll parse it twice
          -- (when handling `didOpen` and again here when handling `didChangeWatchedFiles`),
          -- but that's not a big deal because the file is likely empty anyway.
          --
          -- NOTE ON DIRECTORIES and `Created` events:
          --  * when a folder is created with Ctrl+V
          --     -> we'll get a "created" event for the folder AND for every file within it.
          --  * when a folder is deleted and then re-created with Ctrl+Z
          --     -> we'll get a "created" event for the folder only.
          --  * when a folder is renamed
          --     -> we'll get a "created" event for the folder only.
          -- Because we don't know whether we're going to receive events for the individual files,
          -- we have to assume the worst (we won't). So we traverse the directory and load all files.
          paths <- listPaths uri
          forM_ paths \path -> do
            let uri = LSP.filePathToUri path
            debug logger $ "Loading file from disk: " <> tshow path
            contents <- liftIO $ T.readFile path
            let fileVersion = 1
            modifyState $ pure . Util.loadSymbolsForFile uri contents fileVersion
        LSP.FileChangeType_Deleted -> do
          -- NOTE: We don't know whether this was a file or a directory.
          -- So we have to delete the symbols for this uri, and also delete the symbols for all files with
          -- this uri as a prefix (in case this was a directory).
          debug logger $ "Deleting symbols for file/directory: " <> tshow uri
          modifyState $ Util.deleteSymbolsForFileOrDirectory logger uri

    -- We only send diagnostics after we've processed all file events.
    -- If the symbols didn't change, then the diagnostics won't change either, so we can skip computing diagnostics.
    modifyState \appState1 -> do
      if appState0.symbols == appState1.symbols
        then pure appState1
        else sendDiagnostics logger appState1
  where
    -- This function shadows the top-level `modifyState`.
    -- It's only used here, and it doesn't automatically publish the diagnostics.
    -- Diagnostics are only published after we're done handling all the events.
    modifyState :: (AppState -> AppM AppState) -> AppM ()
    modifyState act = do
      stateVar <- ask
      Unlift.modifyMVar_ stateVar act

    -- When creating a folder, sometimes we might get a "created" event for the folder,
    -- and sometimes we might get "created" events for the folder AND every file within the folder.
    --
    -- To avoid reparsing files unnecessarily, we normalize the events by deduping "created" events.
    -- If we find "created" events for a folder and a file within that folder, we ignore the "created" event for the file.
    dedupFileCreatedEvents :: [LSP.FileEvent] -> [LSP.FileEvent]
    dedupFileCreatedEvents events =
      foldl'
        ( \acc event ->
            if event ^. LSP.type_ == LSP.FileChangeType_Created
              then
                -- If we already have a "created" event for a parent directory, we can ignore this "created" event for the child file.
                if any (\seenEvent -> seenEvent ^. LSP.type_ == LSP.FileChangeType_Created && ((event ^. LSP.uri) `Util.isWithinDir` (seenEvent ^. LSP.uri))) acc
                  then acc
                  else event : acc
              else event : acc
        )
        []
        events
        & reverse

    isFileOpen :: Uri -> AppM Bool
    isFileOpen uri = do
      vf <- getVirtualFile (LSP.toNormalizedUri uri)
      pure $ Maybe.isJust vf

    isFileAndExists :: Uri -> AppM Bool
    isFileAndExists uri =
      case LSP.uriToFilePath uri of
        Nothing -> pure False
        Just fp -> liftIO $ Dir.doesFileExist fp

    -- Ignore files we're not interested in, e.g. `./git` files.
    shouldHandleFile :: Uri -> Bool
    shouldHandleFile uri =
      case LSP.uriToFilePath uri of
        Nothing -> False
        Just fp -> not $ ".git" `elem` FP.splitDirectories fp

    -- If this path points to a file, return it.
    -- If it points to a directory, traverse the directory and return all files within it.
    listPaths :: Uri -> AppM [FilePath]
    listPaths uri =
      case LSP.uriToFilePath uri of
        Nothing -> pure []
        Just fp -> do
          isFile <- liftIO $ Dir.doesFileExist fp
          if isFile
            then pure [fp]
            else do
              isDir <- liftIO $ Dir.doesDirectoryExist fp
              if isDir
                then liftIO $ traverseDir fp
                else pure []
      where
        traverseDir :: FilePath -> IO [FilePath]
        traverseDir dir = do
          contents <- Dir.listDirectory dir
          let paths = contents <&> \name -> dir </> name
          files <- filterM Dir.doesFileExist paths
          dirs <- filterM Dir.doesDirectoryExist paths
          nestedFiles <- mapM traverseDir dirs
          pure $ files <> concat nestedFiles
