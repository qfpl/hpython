{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", profiling ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix { inherit nixpkgs compiler profiling; };

in

  f.env
