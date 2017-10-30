{ pkgs ? import <nixpkgs> { } }:
pkgs.fetchFromGitHub {
  owner = "qfpl";
  repo = "papa";
  rev = "5f8707ca723ec104c79f83a56dfd2fabcc885c06";
  sha256 = "12cb5am618hzydcp372vgbqh2a5a9anx3cbv56dlpf1n51hhxv5f";
}
