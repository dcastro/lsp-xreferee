{-# LANGUAGE MultiWayIf #-}

module Xreferee.Lsp.Util where

import Control.Lens hiding (Indexable, Iso)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map.Strict qualified as SM
import Data.Text qualified as T
import Data.Time.Clock.POSIX qualified as Time
import GHC.IO.Exception (IOErrorType (InappropriateType))
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types (Uri)
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Db (LineNum (..), Symbol (..))
import Xreferee.Lsp.Db qualified as Db
import Xreferee.Lsp.Git (CheckIgnoreResult (..))
import Xreferee.Lsp.Git qualified as Git
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude
import Xreferee.Lsp.Symbols qualified as Symbols

-- The options we use to search for symbols using the `xreferee` package.
searchOpts :: X.SearchOpts
searchOpts =
  X.SearchOpts
    { ignores = [],
      -- When using xreferee in the context of an editor extension (as opposed to using it in e.g. a CI),
      -- we want xreferee to detect changes done to files not yet tracked by git.
      includeUntracked = True,
      delims = X.defaultDelims
    }

-- | Removes the cached symbols for this file and loads the new symbols from the given file contents.
loadSymbolsForFile :: Uri -> LByteString -> Int32 -> AppM ()
loadSymbolsForFile uri contents fileVersion = do
  conn <- view conn

  -- Delete the old symbols for this file.
  Db.deleteSymbolsForFile conn uri

  -- Parse the new symbols for this file.
  forM_ (LBS.lines contents `zip` [0 ..]) \(line, lineNum) -> do
    let (anchors, refs) = X.parseLabels X.defaultDelims line

    forM_ anchors \(anchor, columnRange) -> do
      let symbol = Symbols.mkSymbol anchor uri (LineNum lineNum) columnRange
      Db.insertAnchor conn symbol

    forM_ refs \(ref, columnRange) -> do
      let symbol = Symbols.mkSymbol ref uri (LineNum lineNum) columnRange
      Db.insertReference conn symbol

  -- Update the version we have for this file.
  modifyState \appState1 -> appState1 {fileVersions = SM.insert uri fileVersion appState1.fileVersions}

symbolLocToLspRange :: Symbol -> LSP.Range
symbolLocToLspRange sym =
  LSP.Range
    { _start =
        LSP.Position
          { _line = sym.line.getLineNum,
            _character = sym.columnStart
          },
      _end =
        LSP.Position
          { _line = sym.line.getLineNum,
            _character = sym.columnEnd + 1
          }
    }

symbolLocToLspLocation :: Symbol -> LSP.Location
symbolLocToLspLocation sym =
  LSP.Location
    { _uri = sym.uri,
      _range = symbolLocToLspRange sym
    }

-- | Wraps a request handler to log the time it took to handle the request.
timedReq :: forall from (method :: LSP.Method from 'LSP.Request). LSP.Handler AppM method -> LSP.Handler AppM method
timedReq handler req responder = do
  let method = req ^. LSP.method
  t0 <- liftIO Time.getPOSIXTime
  handler req responder
  t1 <- liftIO Time.getPOSIXTime
  let duration = t1 - t0
  Log.debugP ("Handled " <> tshow method <> " in") duration

-- | Wraps a notification handler to log the time it took to handle the notification.
timedNot :: forall from (method :: LSP.Method from 'LSP.Notification). LSP.Handler AppM method -> LSP.Handler AppM method
timedNot handler req = do
  let method = req ^. LSP.method
  t0 <- liftIO Time.getPOSIXTime
  handler req
  t1 <- liftIO Time.getPOSIXTime
  let duration = t1 - t0
  Log.debugP ("Handled " <> tshow method <> " in") duration

-- | Checks whether we should ignore or process a given file or directory.
--
-- #(ref:shouldHandleFileOrDir)
shouldHandleFileOrDir :: Uri -> AppM Bool
shouldHandleFileOrDir uri = do
  appState0 <- getState
  -- Check if we have this result cached from a previous check.
  case SM.lookup uri appState0.shouldHandleFiles of
    Just should -> pure should
    Nothing -> do
      should <- case LSP.uriToFilePath uri of
        Nothing -> throwIO $ userError $ "Invalid URI: " <> show uri
        Just fp -> liftIO $ doShouldHandleFileOrDir fp

      shouldBool <- case should of
        DoHandle -> pure True
        DontHandle reason -> do
          Log.debug $ "Ignoring file: '" <> uri.getUri <> "' (" <> reason <> ")"
          pure False

      -- Update the cache
      putState $ appState0 {shouldHandleFiles = SM.insert uri shouldBool appState0.shouldHandleFiles}

      pure shouldBool

{-
  Checks whether we should ignore or process a given file or directory.

  We don't handle:
    * Untracked & git-ignored files
    * Binary files
    * The ".git" folder
    * Paths outside the git repo root
    * Symlinks
    * Paths that don't exist on disk
-}
doShouldHandleFileOrDir :: FilePath -> IO ShouldHandle
doShouldHandleFileOrDir fp = do
  result <- runExceptT do
    checkSymlink
    checkUntrackedIgnored
    -- `git check-ignore` will not flag the `.git` folder, so we have to check it manually
    checkIsInGitDir
    checkIsBinaryFile
    pure DoHandle

  pure $ either id id result
  where
    checkSymlink :: ExceptT ShouldHandle IO ()
    checkSymlink = do
      isSymlink <-
        liftIO $
          (Just <$> Dir.pathIsSymbolicLink fp) `catchNoPropagate` \e@(ExceptionWithContext _ inner) ->
            if isDoesNotExistError inner
              then pure Nothing
              else rethrowIO e
      case isSymlink of
        Nothing ->
          throwError $ DontHandle "does not exist"
        Just True ->
          throwError $ DontHandle "symlink"
        Just False ->
          pure ()

    checkUntrackedIgnored :: ExceptT ShouldHandle IO ()
    checkUntrackedIgnored = do
      liftIO (Git.checkIgnore fp) >>= \case
        UntrackedIgnored ->
          -- If it's untracked and git-ignored, we know for sure we don't want to handle it
          throwError $ DontHandle "untracked & git-ignored"
        OutsideRepo ->
          -- If it's outside the repo root, we know for sure we don't want to handle it
          throwError $ DontHandle "outside git repo"
        NotUntrackedIgnored ->
          pure ()

    checkIsInGitDir :: ExceptT ShouldHandle IO ()
    checkIsInGitDir = do
      when (".git" `elem` FP.splitDirectories fp) do
        throwError $ DontHandle "in .git dir"

    checkIsBinaryFile :: ExceptT ShouldHandle IO ()
    checkIsBinaryFile = do
      liftIO (Dir.doesDirectoryExist fp) >>= \case
        True ->
          -- If this is a directory, we short-circuit here.
          -- We don't want to run `git ls-files` on directories,
          -- because it will (unnecessarily) recursively traverse it.
          -- We can't use `--directory` to disable traversal because it's incompatible with `-eol`,
          -- which we need to check whether it's a binary file.
          pure ()
        False -> do
          -- If it's a file, we need to check whether it's a binary file
          liftIO (Git.lsFiles fp) >>= \case
            Nothing ->
              -- The file is not in this git repo
              throwError $ DontHandle "outside git repo"
            Just stdout -> do
              {-
                  When `git ls-files` is run with `--eol`, it'll print file info like this:

                  ```
                  i/      w/none  attr/                   file2.md
                  i/      w/-text attr/                   lsp-xreferee.eventlog
                  i/      w/lf    attr/                   lsp-xreferee.hp
                  i/      w/lf    attr/                   lsp-xreferee.prof
                  i/none  w/none  attr/                   file.md
                  ```

                  Binary files will be marked with `w/-text` in the output.
              -}
              let isBinary = "w/-text" `T.isInfixOf` stdout
              when isBinary do
                throwError $ DontHandle "binary file"

data ShouldHandle
  = DoHandle
  | DontHandle Text
  deriving stock (Show, Eq)

-- | Reads a file's contents, or returns `Nothing` if the file no longer exists
-- or is actually a directory.
--
-- >>> import Data.Either (isRight)
-- >>> isRight <$> readFileIfExists "README.md"
-- True
-- >>> readFileIfExists "invalid.md"
-- Left RFNotExists
-- >>> readFileIfExists "src"
-- Left RFIsDirectory
readFileIfExists :: (MonadIO m) => FilePath -> m (Either ReadFileError LBS.ByteString)
readFileIfExists fp =
  liftIO $
    (Right <$> LBS.readFile fp) `catchNoPropagate` \e@(ExceptionWithContext _ inner) ->
      if
        | isDoesNotExistError inner -> pure (Left RFNotExists)
        | ioeGetErrorType inner == InappropriateType -> pure (Left RFIsDirectory)
        | otherwise -> rethrowIO e

data ReadFileError
  = RFNotExists
  | RFIsDirectory
  deriving stock (Show, Eq)
