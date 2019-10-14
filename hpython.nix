{ mkDerivation, base, bifunctors, bytestring, containers, criterion
, deepseq, deriving-compat, digit, dlist, filepath, fingertree
, generic-lens, hedgehog, lens, megaparsec, mtl, parsers
, parsers-megaparsec, semigroupoids, stdenv, text, these
, validation
}:
mkDerivation {
  pname = "hpython";
  version = "0.3";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors bytestring containers deriving-compat digit dlist
    fingertree generic-lens lens megaparsec mtl parsers
    parsers-megaparsec semigroupoids text these validation
  ];
  executableHaskellDepends = [ base lens text ];
  testHaskellDepends = [
    base filepath hedgehog lens megaparsec text validation
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq megaparsec text validation
  ];
  description = "Python language tools";
  license = stdenv.lib.licenses.bsd3;
}
