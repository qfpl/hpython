{ pkgs ? import <nixpkgs> { }, haskellPackages }:
pkgs.fetchFromGitHub {
  owner = "qfpl";
  repo = "papa";
  rev = "f76316938c1d5398cc9ab01de12bfde6b68b8502";
  sha256 = "14dlk0v2p6y65953bnmy6bm3m5a5fag209r53kq6ycwqw8m0m1ja";
}
