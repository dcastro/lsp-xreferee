# Changelog for `lsp-xreferee`

All notable changes to this project will be documented in this file.

## Unreleased

* Add backtraces to logged exceptions.
* Dump to a logfile when the server crashes.
* Display runtime exceptions in the client.
* Bug fixes:
  * Scan tracked & git-ignored files, see: https://github.com/brandonchinn178/xreferee/issues/27
  * Treat paths as literal pathspecs when checking whether a file is binary via `git ls-files`
  * Fixed race conditions when files were deleted after an event is triggered and before it's done being handled
  * Don't follow symlinks (to match `xreferee`'s behavior)
  * When a folder is created, individually check whether each file should be handled or ignored
  * Correctly handle file deletion
  * Correctly handle file/folder deletion on Windows

## 0.0.2

* Upgraded to [`xreferee v1.1.1`](https://github.com/brandonchinn178/xreferee/releases/tag/v1.1.1)
* Added support for opening workspaces in a git repo's subdirectories.
* Reload all anchors/references when `.gitignore` changes.
* Bug fix: correctly handle all Unicode characters.
* Performance improvements:
  * Cached `git check-ignore` results.
  * When handling `workspace/didChangeWatchedFiles`, push the new diagnostics only once, after every file event has been processed.
  * Ignore binary files

## 0.0.1

Initial release
