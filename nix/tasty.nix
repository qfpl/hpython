{ mkDerivation, ansi-terminal, async, base, clock, containers
, deepseq, mtl, optparse-applicative, regex-tdfa, stdenv, stm
, tagged, unbounded-delays, unix
}:
mkDerivation {
  pname = "tasty";
  version = "0.12.0.1";
  sha256 = "eb03d1f961f2e1439be96c44287b090058d6cb9be3dd9000f722ff35c9c2b0e3";
  libraryHaskellDepends = [
    ansi-terminal async base clock containers deepseq mtl
    optparse-applicative regex-tdfa stm tagged unbounded-delays unix
  ];
  homepage = "https://github.com/feuerbach/tasty";
  description = "Modern and extensible testing framework";
  license = stdenv.lib.licenses.mit;
}
