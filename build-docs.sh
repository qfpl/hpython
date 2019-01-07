#!/bin/sh

nix-shell --run "cabal new-haddock --ghc-options=-fforce-recomp && cp -R dist-newstyle/build/x86_64-linux/ghc-8.4.4/hpython-0.1.0.1/doc/html/hpython/* docs"
