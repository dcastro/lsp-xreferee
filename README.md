# lsp-xreferee

This is an LSP server for running [xreferee](https://github.com/brandonchinn178/xreferee) on a git workspace.

It is used by the [xreferee VSCode extension](https://github.com/dcastro/vscode-xreferee).

The latest release for Linux / macOS / Windows can be found in the [Releases page](https://github.com/dcastro/lsp-xreferee/releases).

## Features

* Go To Definition: Navigate from a reference to an anchor.
* Find All References: Navigate from an anchor to its references.
* Diagnostics: report warnings/errors for unused anchors, duplicate anchors, and broken references.
* Rename labels, updating all associated anchors/refs.
