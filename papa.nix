with (import <nixpkgs> { }).pkgs;
import (fetchFromGitHub {
  repo = "papa";
  owner = "qfpl";
  rev = "05bf21fad6176eed1c9bbfbe08f1f058447a510b";
  sha256 = "0ggv9c789l473k8hxsqcp6mnncw38qkv4v5hjpfln2l479ljafxx";
})
