{ pkgs ? import <nixpkgs> { }, haskellPackages }:
haskellPackages.callPackage
  (pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "digit";
    rev = "9e0c69abc01150c5e5a6af295cb290e6b4dc1009";
    sha256 = "1w5qw3v4875r9rmgbzdhzvggaqhaskjadlxk8qpzypfl872i4pbf";
  })
  {}
