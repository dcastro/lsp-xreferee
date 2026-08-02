{-# LANGUAGE MultiWayIf #-}

module Xreferee.Lsp.Util where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Map.Strict qualified as SM
import Data.Text qualified as T
import GHC.IO.Exception (IOErrorType (InappropriateType))
import Language.LSP.Protocol.Types qualified as LSP
import System.Directory qualified as Dir
import System.FilePath qualified as FP
import XReferee.SearchResult qualified as X
import Xreferee.Lsp.AppM
import Xreferee.Lsp.Git (CheckIgnoreResult (..))
import Xreferee.Lsp.Git qualified as Git
import Xreferee.Lsp.Log qualified as Log
import Xreferee.Lsp.Prelude

-- The options we use to search for symbols using the `xreferee` package.
searchOpts :: Config -> X.SearchOpts
searchOpts cfg =
  X.SearchOpts
    { ignores = cfg.ignore,
      -- When using xreferee in the context of an editor extension (as opposed to using it in e.g. a CI),
      -- we want xreferee to detect changes done to files not yet tracked by git.
      includeUntracked = True,
      delims = X.defaultDelims
    }

-- | Appends a trailing path separator to a uri, unless it already has one.
--
-- This is needed whenever we want to check whether a uri is contained within a
-- directory: without the trailing separator, `./foobar/file.md` would incorrectly
-- be considered to be within `./foo`.
--
-- NOTE: We deliberately don't use `System.FilePath.addTrailingPathSeparator` here,
-- because it appends the platform's path separator, which is `\` on Windows.
-- Uris always use `/`, regardless of the platform.
--
-- #(ref:uriAddTrailingPathSeparator)
uriAddTrailingPathSeparator :: Uri -> Text
uriAddTrailingPathSeparator uri =
  if "/" `T.isSuffixOf` uri.getUri
    then uri.getUri
    else uri.getUri <> "/"

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
        -- `uriToFilePath` returns `Nothing` for any URI that doesn't map to a
        -- filesystem path, i.e. anything whose scheme isn't `file:`. VSCode's
        -- built-in Git extension routinely sends us events for virtual documents
        -- under the `git:` scheme (used for diff/timeline views), and other
        -- extensions use schemes like `untitled:` or `vscode-*`. None of these
        -- exist on disk, so there's nothing for us to process. This is expected,
        -- not an error, so we quietly decline to handle them rather than throwing.
        Nothing -> pure $ DontHandle "non-file URI scheme"
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

  NOTE: Paths that don't exist on disk ARE not necessarily excluded.
  This function is also used before we handle `FileChangeType_Deleted` events.

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
          -- File does not exist
          pure ()
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
-- WARNING: this does lazy IO, so the file may be deleted or changed after this function returns,
-- and thus throw an exception while the bytestring is being consumed.
--
-- Also, the bytestring MUST be read to completion, otherwise the file handle will remain open until garbage collected.
-- See the docs for `hGetContents`: https://hackage-content.haskell.org/package/bytestring-0.12.2.0/docs/Data-ByteString-Lazy.html#v:hGetContents
-- > Chunks are read on demand, using the default chunk size.
-- > File handles are closed on EOF if all the file is read, or through garbage collection otherwise.
--
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
    -- This uses lazy file IO, so we can only catch exceptions from OPENING the file,
    -- not from reading it.
    (Right <$> LBS.readFile fp) `catchNoPropagate` \e@(ExceptionWithContext _ inner) ->
      if
        | isDoesNotExistError inner -> pure (Left RFNotExists)
        | ioeGetErrorType inner == InappropriateType -> pure (Left RFIsDirectory)
        | otherwise -> rethrowIO e

data ReadFileError
  = RFNotExists
  | RFIsDirectory
  deriving stock (Show, Eq)
