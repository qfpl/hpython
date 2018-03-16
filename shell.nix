{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix { inherit nixpkgs compiler; };

in

  f.env
