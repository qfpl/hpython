{ pkgs ? import <nixpkgs> { }, haskellPackages }:
haskellPackages.callPackage
  (pkgs.fetchFromGitHub {
    owner = "qfpl";
    repo = "tasty-hedgehog";
    rev = "0.1.0.1";
    sha256 = "04pmr9q70gakd327sywpxr7qp8jnl3b0y2sqxxxcj6zj2q45q38m";
  })
  {}
