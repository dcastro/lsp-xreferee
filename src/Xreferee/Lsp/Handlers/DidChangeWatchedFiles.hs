{-# LANGUAGE MultiWayIf #-}

module Xreferee.Lsp.Handlers.DidChangeWatchedFiles
  ( handleDidChangeWatchedFiles,
    FileEvent (..),
    FileChangeType (..),
    mkFileEvent,
    dedupeEvents,
  )
where

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

mkFileEvent :: LSP.FileEvent -> FileEvent
mkFileEvent event =
  FileEvent
    { uri = event ^. LSP.uri,
      eventType =
        case event ^. LSP.type_ of
          LSP.FileChangeType_Deleted -> Deleted
          LSP.FileChangeType_Created -> CreatedOrChanged
          LSP.FileChangeType_Changed -> CreatedOrChanged,
      originalEventType = event ^. LSP.type_
    }

-- | https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workspace_didChangeWatchedFiles
--
-- Watches for file changes in the git repo that happen outside the editor
-- (e.g. when the user switches git branches, or the user deletes a file via the file manager).
handleDidChangeWatchedFiles :: Handler AppM 'LSP.Method_WorkspaceDidChangeWatchedFiles
handleDidChangeWatchedFiles = \req -> do
  annotateStackStringIO "handleDidChangeWatchedFiles" do
    Log.logNot req

    let fileEvents =
          req
            ^.. LSP.params
              . LSP.changes
              . each
              . to mkFileEvent
    let (dedupedFileEvents, droppedEvents) = dedupeEvents fileEvents

    for_ droppedEvents \droppedEvent -> do
      Log.debug $ "Dropped file event: " <> tshow droppedEvent.originalEventType <> " for " <> droppedEvent.uri.getUri

    for_ dedupedFileEvents \event -> do
      let logMsg = "Handling file event: " <> tshow event.originalEventType <> " for " <> event.uri.getUri
      annotateStackStringIO (unpack logMsg) do
        Log.debug logMsg
        whenM (Util.shouldHandleFileOrDir event.uri) do
          handleFileEvent event

-- | Proccess a filesystem event.
--
-- This function continously updates the app state, without pushing diagnostics to the client.
-- We only push diagnostics to the client after we've processed all file events.
handleFileEvent :: FileEvent -> AppM ()
handleFileEvent evt =
  case evt.eventType of
    CreatedOrChanged -> do
      paths <- listPaths evt.uri
      for_ paths \path -> do
        let uri = LSP.filePathToUri path
        -- Check if we should handle events for this file
        whenM (Util.shouldHandleFileOrDir uri) do
          -- Check if this file is open. If it is, we don't handle the event.
          -- If the filesystem and the editor buffer are out of sync, the editor buffer takes priority, it's the source of truth.
          -- See @(ref:check-is-open)
          whenM (not <$> isFileOpen uri) do
            Util.readFileIfExists path >>= \case
              Left RFNotExists -> do
                -- NOTE: the file may have been deleted since we listed it, so we skip it if it's gone.
                Log.debug $ "[WARN] didChangeWatchedFiles: CreatedOrChanged: file was deleted: " <> tshow path
                pure ()
              Left RFIsDirectory -> do
                -- This should never happen, because we already filtered out directories in `listPaths`.
                -- But just in case (e.g. the path was quickly changed from a file to a directory),
                -- we skip it.
                Log.debug $ "[WARN] didChangeWatchedFiles: CreatedOrChanged: path is a directory: " <> tshow path
                pure ()
              Right contents -> do
                Log.debug $ "didChangeWatchedFiles: CreatedOrChanged: loading file from disk: " <> tshow path
                Symbols.refreshSymbolsForFile uri contents
    Deleted -> do
      filesWithSymbols <- Db.findFilesInPathWithSymbols evt.uri
      for_ filesWithSymbols \uri -> do
        -- Check if we should handle events for this file
        whenM (Util.shouldHandleFileOrDir uri) do
          -- Check if this file is open. If it is, we don't handle the event.
          -- If the filesystem and the editor buffer are out of sync, the editor buffer takes priority, it's the source of truth.
          -- See @(ref:check-is-open)
          whenM (not <$> isFileOpen uri) do
            -- If the file exists on disk, skip this.
            -- See @(ref:delete-commutative)
            let path = LSP.uriToFilePath uri & fromMaybe (error $ "Invalid URI stored in the database: " <> unpack uri.getUri)
            whenM (not <$> liftIO (Dir.doesFileExist path)) do
              Log.debug $ "didChangeWatchedFiles: Deleted: Deleting symbols for file/directory: " <> uri.getUri
              Db.deleteSymbolsForFile uri
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
