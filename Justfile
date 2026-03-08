# Just list all recipes by default
default:
    just --list

# Build and install the LSP when file changes, and emit a KDE notification when done
filewatch:
    watchexec --clear --restart \
      --exts hs \
      -- 'stack install --fast ; kdialog --passivepopup Done'
