{ pkgs ? import <nixpkgs> { } }:
pkgs.fetchFromGitHub {
  owner = "qfpl";
  repo = "digit";
  rev = "c02aadb4a471a357c5f93d0db40b5f26443dc344";
  sha256 = "0rhzgqhkxn3rjpmnq4pwh7kfphc99z67wwh2vf2q1d48pnizw2j5";
}
