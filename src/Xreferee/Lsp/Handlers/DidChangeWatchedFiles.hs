{-# LANGUAGE MultiWayIf #-}

module Xreferee.Lsp.Handlers.DidChangeWatchedFiles where

import Control.Lens hiding (Indexable, Iso)
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server as LSP
import System.Directory qualified as Dir
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols
import Xreferee.Lsp.Util (ReadFileError (..))
import Xreferee.Lsp.Util qualified as Util

data FileEvent = FileEvent
  { uri :: Uri,
    eventType :: FileChangeType,
    -- The original event type from the LSP notification. This is used for logging and debugging.
    originalEventType :: LSP.FileChangeType
  }
  deriving stock (Show, Eq)

data FileChangeType
  = -- | We treat "created" and "changed" events the same way, see @(ref:changed-created-equivalency).
    CreatedOrChanged
  | Deleted
  deriving stock (Show, Eq)

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
--
-- Watches for file changes in the git repo that happen outside the editor
-- (e.g. when the user switches git branches, or the user deletes a file via the file manager).
handleDidChangeWatchedFiles :: Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles
handleDidChangeWatchedFiles = \req -> do
  annotateStackStringIO "handleDidChangeWatchedFiles" do
    Log.logNot req
    let fileEvents = dedupFileCreatedEvents $ req ^. LSP.params . LSP.changes
    runHandler fileEvents
  where
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
                if any (\seenEvent -> seenEvent ^. LSP.type_ == LSP.FileChangeType_Created && ((event ^. LSP.uri) `isWithinDir` (seenEvent ^. LSP.uri))) acc
                  then acc
                  else event : acc
              else event : acc
        )
        []
        events
        & reverse

    -- Checks if a URI points to a file within a given directory.
    isWithinDir :: Uri -> Uri -> Bool
    isWithinDir file dir =
      -- We MUST add a trailing path separator.
      -- Otherwise, `isWithinDir ./foobar/file.md ./foo` would incorrectly be `True`.
      Util.uriAddTrailingPathSeparator dir `T.isPrefixOf` file.getUri

-- | Proccess a list of file events.
--
-- This function continously updates the app state, without pushing diagnostics to the client.
-- We only push diagnostics to the client after we've processed all file events.
runHandler :: [LSP.FileEvent] -> AppM ()
runHandler fileEvents = do
  forM_ fileEvents \fileEvent -> do
    let uri = fileEvent ^. LSP.uri
    let eventType = fileEvent ^. LSP.type_
    let logMsg = "Handling file event: " <> tshow eventType <> " for " <> uri.getUri

    annotateStackStringIO (unpack logMsg) do
      Log.debug logMsg
      whenM (Util.shouldHandleFileOrDir uri) do
        case eventType of
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
            -- In other words: in all situations where the file changes AND is open in the editor, we do NOT need to handle this event.
            --  I.e., we only care about this event if the file is NOT open in the editor.
            whenM (not <$> lift (isFileOpen uri)) do
              Util.readFileIfExists (LSP.uriToFilePath uri & Maybe.fromJust) >>= \case
                Left RFNotExists -> do
                  -- NOTE: the file may have been deleted between the event being triggered and reaching here, so we skip it if it's gone.
                  Log.debug $ "[WARN] didChangeWatchedFiles: Changed: file was deleted: " <> uri.getUri
                  pure ()
                Left RFIsDirectory -> do
                  -- We'll get "changed" events for directories if e.g. the user sets attributes or changes permissions on the directory.
                  -- We should ignore those events.
                  Log.debug $ "didChangeWatchedFiles: Changed: path is a directory: " <> uri.getUri
                  pure ()
                Right contents -> do
                  Log.debug $ "didChangeWatchedFiles: Changed: reloading file from disk: " <> uri.getUri
                  Symbols.loadSymbolsForFile uri contents
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
              whenM (Util.shouldHandleFileOrDir uri) do
                Util.readFileIfExists path >>= \case
                  Left RFNotExists -> do
                    -- NOTE: the file may have been deleted since we listed it, so we skip it if it's gone.
                    Log.debug $ "[WARN] didChangeWatchedFiles: Created: file was deleted: " <> tshow path
                    pure ()
                  Left RFIsDirectory -> do
                    -- This should never happen, because we already filtered out directories in `listPaths`.
                    -- But just in case (e.g. the path was quickly changed from a file to a directory),
                    -- we skip it.
                    Log.debug $ "[WARN] didChangeWatchedFiles: Created: path is a directory: " <> tshow path
                    pure ()
                  Right contents -> do
                    Log.debug $ "didChangeWatchedFiles: Created: loading file from disk: " <> tshow path
                    Symbols.loadSymbolsForFile uri contents
          LSP.FileChangeType_Deleted -> do
            -- NOTE: We don't know whether this was a file or a directory.
            -- So we have to delete the symbols for this uri, and also delete the symbols for all files with
            -- this uri as a prefix (in case this was a directory).
            Log.debug $ "didChangeWatchedFiles: Deleted: Deleting symbols for file/directory: " <> uri.getUri
            Db.deleteSymbolsForFileOrDirectory uri
  where
    isFileOpen :: (MonadLsp Config m) => Uri -> m Bool
    isFileOpen uri = do
      vf <- getVirtualFile (LSP.toNormalizedUri uri)
      pure $ Maybe.isJust vf

    -- If this path points to a file, return it.
    -- If it points to a directory, traverse the directory and return all files within it.
    listPaths :: (MonadIO m) => Uri -> m [FilePath]
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

-- | If we get a set of events with a `CreatedOrChanged` events for a folder and N `CreatedOrChanged` events for files inside that folder,
-- those N events may be dropped.
--
-- Returns a tuple of (deduped list, dropped events).
--
-- See: @(ref:dedupe-events)
dedupeEvents :: [FileEvent] -> ([FileEvent], [FileEvent])
dedupeEvents events =
  foldr
    ( \event (events, dropped) ->
        let (updatedEvents, mbDropped) = addOrReplaceEvent event events
         in (updatedEvents, maybeToList mbDropped <> dropped)
    )
    ([], [])
    events
  where
    -- If this event E1 is a parent directory of some existing event in the list E2, drop E2 and replace it with E1.
    -- If this event E1 is a child of some existing event in the list E2, drop E1.
    -- Otherwise, add E1 to the list.
    --
    -- Returns the dropped element, if any.
    addOrReplaceEvent :: FileEvent -> [FileEvent] -> ([FileEvent], Maybe FileEvent)
    addOrReplaceEvent event events =
      case event.eventType of
        Deleted ->
          -- No deduping needed, just append the event to the accumulator.
          (event : events, Nothing)
        CreatedOrChanged ->
          let (updatedEvents, res) =
                foldr
                  ( \seenEvent (acc, res) ->
                      if
                        -- If we've already replaced an event in the list, then skip checking all the others events.
                        | wasDuplicateFound res -> (seenEvent : acc, res)
                        | seenEvent.eventType /= CreatedOrChanged -> (seenEvent : acc, res)
                        | event.uri `isParentDirOf` seenEvent.uri -> (acc, IsParentOf seenEvent)
                        | seenEvent.uri `isParentDirOf` event.uri -> (seenEvent : acc, IsChild)
                        | otherwise -> (seenEvent : acc, res)
                  )
                  ([], NoDuplicates)
                  events
           in case res of
                NoDuplicates -> (event : updatedEvents, Nothing)
                IsParentOf dropped -> (event : updatedEvents, Just dropped)
                IsChild -> (updatedEvents, Just event)

-- The result of checking whether a file event E is a duplicate of an existing event in the list.
data DropResult
  = -- E is not a duplicate of any existing event in the list.
    NoDuplicates
  | -- E's path is a parent directory of an existing event in the list, so the existing event should be dropped.
    IsParentOf FileEvent
  | -- E's path is a child directory of an existing event in the list, so E should be dropped.
    IsChild

wasDuplicateFound :: DropResult -> Bool
wasDuplicateFound = \case
  NoDuplicates -> False
  IsParentOf _ -> True
  IsChild -> True

-- | Checks if a URI is a parent directory of another URI.
--
-- >>>  LSP.filePathToUri "./foo" `isParentDirOf` LSP.filePathToUri "./foo/bar/file.md"
-- True
-- >>>  LSP.filePathToUri "./foo" `isParentDirOf` LSP.filePathToUri "./foobar/file.md"
-- False
-- >>> LSP.filePathToUri "./foo/bar/file.md" `isParentDirOf` LSP.filePathToUri "./foo"
-- False
isParentDirOf :: Uri -> Uri -> Bool
isParentDirOf parentDir file =
  -- We MUST add a trailing path separator.
  -- Otherwise, @./foo `isParentDirOf` ./foobar/file.md@ would incorrectly be @True@.
  Util.uriAddTrailingPathSeparator parentDir `T.isPrefixOf` Util.uriAddTrailingPathSeparator file
