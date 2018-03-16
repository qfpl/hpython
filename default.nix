{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./hpython.nix;


  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  type-level-sets = haskellPackages.callPackage ./nix/type-level-sets.nix {};

  drv = haskellPackages.callPackage f { inherit type-level-sets; };

in

  pkgs.haskell.lib.dontHaddock drv
