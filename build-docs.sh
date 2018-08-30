#!/bin/sh

nix-shell --run "cabal new-haddock && cp -R dist-newstyle/build/x86_64-linux/ghc-8.2.2/hpython-0.1.0.0/doc/html/hpython/* docs"
