{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  drv = import ./default.nix { inherit nixpkgs compiler; };
in
  if nixpkgs.lib.inNixShell then drv.env else drv
