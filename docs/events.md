# Notes


* When vscode starts, we'll get a `didOpen` event for the tab currently focused AND any other tab with unsaved changes.

* We can receive `didOpen` / `didChange` events for files that don't YET exist on disk. E.g.:
  1. The user opens a new tab, makes some changes, and saves the file.
     When the file is saved, we'll get `didOpen -> didChange -> FileChangeType_Created -> FileChangeType_Changed` in quick succession, in that order.
  2. The user opens a file, and renames it from within the editor.
     We'll get `didClose -> didOpen -> FileChangeType_Created -> FileChangeType_Deleted`

* We can receive `FileChangeType_Created` events for files that are already open in the editor.
  1. See the example above.
  2. But also:
    * say we open a file in the editor
    * then delete it from the filesystem (and the editor tab stays open)
    * then we recreate it on the filesystem (e.g. via `touch` or by saving the file in the editor), we'll get a `FileChangeType_Created`

## Deleting a file currently open

* When a file is open in the editor and is then deleted:
  * Events:
    * We will get a `didChangeWatchedFiles` event with `FileChangeType_Deleted` (as expected).
    * But we will NOT get a `didClose` event.
  * Note that if the user keeps editing this tab, the server will still receive `didChange` events, even though the file does not exist on disk.
  * We have 2 possible solutions:
    1. Delete the symbols right away, even though the file is still open in the editor
    2. Keep the symbols loaded. Update them when `didChange` events are received. Delete them when `didClose` is sent and if the file STILL doesn't exist on disk. (#(ref:didClose-deleted-file))

Initially, our implementation went with solution 1.

But that's not consistent with a core principle of the LSP server: when the editor buffer and the filesystem diverge, the buffer always wins.
We should always treat the editor buffer as the source of truth.

SO we need to switch to solution 2: if the tab is still open, its symbols must be preserved.

## Replacing folders

* When a folder is "replaced" on disk with e.g. `rm -rf ./test ; mv ../some-dir ./test`
  * Events:
    * We'll get a `FileChangeType_Changed` for the folder itself
    * We'll get a `FileChangeType_Deleted` for the files that were deleted
    * We'll get a `FileChangeType_Deleted` for the files that were replaced
    * We will NOT get any events for new files
  * In this case, we want to:
    * `to_replace.md`:
      * If it's open, skip the event
      * If it's not open, delete all symbols
    * `to_delete.md`:
      * If it's open, skip the event
      * If it's not open, delete all symbols
    * `test` folder:
      * traverse the directory, list all files
      * `to_create.md`: If the file is open, skip the event. Otherwise, parse it
      * `to_replace.md`: If the file is open, skip the event. Otherwise, parse it

```sh
# setup
^rm -rf ../test ; mkdir ../test ; touch ../test/to_replace.md ; touch ../test/to_create.md
^rm -rf ./test ; mkdir ./test ; touch ./test/to_replace.md ; touch ./test/to_delete.md

# delete and mv at the same time.
^rm -rf ./test ; mv ../test ./test
```
```hs
#(ref:example1)
[ FileEvent
    { _uri = Uri
        { getUri = "file:///home/dc/Dropbox/Projects/xreferee/example-xreferee/test" }
    , _type_ = FileChangeType_Changed
    }
, FileEvent
    { _uri = Uri
        { getUri = "file:///home/dc/Dropbox/Projects/xreferee/example-xreferee/test/to_delete.md" }
    , _type_ = FileChangeType_Deleted
    }
, FileEvent
    { _uri = Uri
        { getUri = "file:///home/dc/Dropbox/Projects/xreferee/example-xreferee/test/to_replace.md" }
    , _type_ = FileChangeType_Deleted
    }
]
```

## Creating folders

If we create a folder _with files_ with `mv`, we'll get a single `FileChangeType_Created` event for the folder.
If we do it with `cp -r`, we'll get 2x `FileChangeType_Created` events: one for the folder and another for the file.

With `mv`:

```sh
# Setup
^rm -rf ../test2 ; mkdir ../test2 ; touch ../test2/to_create.md ; ^rm -rf ./test2
mv ../test2 ./test2
```
```hs
[ FileEvent
    { _uri = Uri
        { getUri = "file:///home/dc/Dropbox/Projects/xreferee/example-xreferee/test2" }
    , _type_ = FileChangeType_Created
    }
]
```

With `cp -r`:
```sh
# Setup
^rm -rf ../test2 ; mkdir ../test2 ; touch ../test2/to_create.md ; ^rm -rf ./test2
cp -r ../test2 ./test2
```
```hs
[ FileEvent
    { _uri = Uri
        { getUri = "file:///home/dc/Dropbox/Projects/xreferee/example-xreferee/test2" }
    , _type_ = FileChangeType_Created
    }
, FileEvent
    { _uri = Uri
        { getUri = "file:///home/dc/Dropbox/Projects/xreferee/example-xreferee/test2/to_create.md" }
    , _type_ = FileChangeType_Created
    }
]
```

This means:
* We can't rely on there being events for specific files, we have to handle events for folders and then recursively traverse the folder to find its files.
* As a performance optimization, we should dedupe `FileChangeType_Created` events for folders + files when they're sent together.
  * #(ref:dedupe-example)

## Renaming files

When a file is renamed via the editor, AND it's open, we'll get:
  * didClose -> didOpen -> FileChangeType_Created + FileChangeType_Deleted

When a file is renamed via the editor, AND it's not open, we'll get:
  * FileChangeType_Created + FileChangeType_Deleted

When a file is renamed via the filesystem, (regardless if it's open), we'll get:
  * FileChangeType_Created + FileChangeType_Deleted

# Implementation

* didOpen
  * consider that the file may not exist on disk
* didChange
  * consider that the file may not exist on disk
* didClose
  * check if the file still exists on disk. if it doesn't, delete all its symbols from the db.
  * See @(ref:didClose-deleted-file)

* All filesystem events, handled by `didChangeWatchedFiles`, **MUST** check if the file is currently open. If it is, skip the event. The editor is the source of truth.
  * When we receive a filesystem event for a **folder**, we must traverse the folder and then apply this check to each individual file.

* The filesystem events `FileChangeType_Changed` and `FileChangeType_Created` _may_ be treated the same way:
  * For files:
    * it doesn't matter whether it was changed or created, we have to parse it from scratch.
    * For `FileChangeType_Changed` events, we don't need to delete existing symbols, so we may differentiate between these 2 events as a performance optimization.
  * For folders:
    * `FileChangeType_Changed` can actually mean that new files were created (see @(ref:example1)), so we **MUST** treat it as `FileChangeType_Created`

* Filesystem events must be handled in reverse order (right to left)
  * See @(ref:example1): handling those events from left to right would mean we end up deleting `to_replace.md`

* The filesystem events `FileChangeType_Changed` and `FileChangeType_Created` _may_ be deduplicated, as a performance optimization:
  * See @(ref:dedupe-example)
  * If such an event for a folder is followed by a `n` events for files inside that folder, those `n` events may be dropped.
  * Note there can files/folders can be arbitrarily nested, e.g. we can receive a list of events for `[ ./dir, ./dir/file1.md, ./dir/deep, ./dir/deep/file2.md ]`
