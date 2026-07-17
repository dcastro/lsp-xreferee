# Just list all recipes by default
default:
    just --list

# Build and install the LSP when files change, and emit a KDE notification when done
filewatch:
    watchexec --clear --restart \
      --exts hs,yaml \
      -- 'xreferee && stack install --ghc-options="-O2" ; kdialog --passivepopup Done'

test:
    stack test

test-filter filter:
    stack test --fast --test-arguments='--filter "{{ filter }}"' --file-watch
