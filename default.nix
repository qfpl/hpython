with import <nixpkgs> { };
let
  haskellPackages = pkgs.haskell.packages.ghc802;
in
haskellPackages.callPackage
  ./hpython.nix
  { inherit (import ./papa.nix haskellPackages); }
