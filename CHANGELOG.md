# Changelog for `lsp-xreferee`

All notable changes to this project will be documented in this file.

## Unreleased

* Dump to a logfile when the server crashes.

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
