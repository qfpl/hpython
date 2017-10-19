{ nixpkgs ? import <nixpkgs> { }, compiler ? "default" }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage
    ./hpython.nix
    { digit =
        haskellPackages.callPackage
          (import ./nix/digit.nix { inherit pkgs; })
          {};
    };

  drvWithPython =
    drv.overrideAttrs (oldAttrs: oldAttrs // {
      buildInputs = oldAttrs.buildInputs ++ [ pkgs.python3 ];
    });

in drvWithPython
