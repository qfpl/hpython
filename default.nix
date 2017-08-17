with import <nixpkgs> { };
let
  latest =
    import (fetchTarball https://github.com/nixos/nixpkgs-channels/archive/129f8d7e999b1a1f0fceaecadca30211e34d85a6.tar.gz) {};
in
latest.haskellPackages.callPackage
  ./hpython.nix
  {
    inherit (import ./papa.nix latest.haskellPackages);
    tasty-hedgehog = import ../tasty-hedgehog { pkgs = latest; };
  }
