# Just list all recipes by default
default:
    just --list

# Build and install the LSP when files change, and emit a KDE notification when done
filewatch:
    watchexec --clear --restart \
      --exts hs,yaml,cabal \
      -- 'xreferee && cabal install --ghc-options="-O2" --overwrite-policy=always ; kdialog --passivepopup Done'

test:
    cabal test

test-filter filter:
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
    cabal clean && cabal build all --enable-tests --enable-benchmarks --ghc-options "-Werror"
