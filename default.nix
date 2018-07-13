{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./hpython.nix;


  haskellPackages =
    (if compiler == "default"
     then pkgs.haskellPackages
     else pkgs.haskell.packages.${compiler}).override {
       overrides = self: super: {
         free = self.free_5_0_1;
         kan-extensions = self.kan-extensions_5_1;
         semigroupoids = self.semigroupoids_5_2_2;
         adjunctions = self.adjunctions_4_4;
         lens = self.lens_4_16;
         deriving-compat = self.deriving-compat_0_4_1;
         type-level-sets = self.callPackage ./nix/type-level-sets.nix {};
         digit = self.callPackage ./nix/digit.nix {};
       };
     };

  drv = haskellPackages.callPackage f {};

in

  pkgs.haskell.lib.dontHaddock drv
