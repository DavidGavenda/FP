# FP - Tím 12
  Peter Andrejko
  Dávid Gavenda
  Marek Dráb
  Marek Klímo

stack exec -- pacman --noconfirm -Sy msys2-keyring

stack exec -- pacman --noconfirm -S mingw-w64-x86_64-icu

stack exec -- pacman --noconfirm -S mingw-w64-x86_64-pkg-config

stack build

stack run
