{ pkgs ? import <nixpkgs> { }, compiler ? "ghc802" }:
pkgs.haskell.packages.${compiler}.callPackage
  ./hpython.nix
  { inherit (import ./papa.nix haskellPackages); }
