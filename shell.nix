{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage
    ./hpython.nix
    { tasty-hedgehog =
        import ./nix/tasty-hedgehog.nix { inherit pkgs haskellPackages; };
      digit = import ./nix/digit.nix { inherit pkgs haskellPackages; };
      papa = import ./nix/papa.nix { inherit pkgs haskellPackages; };
    };

in

  if pkgs.lib.inNixShell then drv.env else drv
