# Just list all recipes by default
default:
    just --list

# Build and install the LSP when files change, and emit a KDE notification when done
filewatch:
    watchexec --clear --restart \
      --exts hs,yaml,cabal \
      -- 'xreferee && cabal install --ghc-options="-O2" --overwrite-policy=always ; kdialog --passivepopup Done'

test:
    # NOTE: The LSP tests rely on `lsp-xreferee` being on PATH, so we have to install it first
    cabal test

test-filter filter:
    # NOTE: The LSP tests rely on `lsp-xreferee` being on PATH, so we have to install it first
    watchexec --clear --restart \
      --exts hs,yaml,cabal \
      -- 'cabal test --test-options="--filter \"{{ filter }}\""'

install:
    cabal install --overwrite-policy=always

format:
    ormolu --mode inplace $(git ls-files -- '*.hs' ':!:src/ExceptionUtil.hs')

checks:
    xreferee
    just test
    just format
    cabal build all --enable-tests --enable-benchmarks --ghc-options "-Werror"
