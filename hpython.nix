{ mkDerivation, base, bifunctors, bytestring, containers, criterion
, deepseq, deriving-compat, digit, directory, dlist, filepath
, fingertree, hedgehog, lens, megaparsec, mtl, parsers
, parsers-megaparsec, process, semigroupoids, stdenv, text, these
, transformers, validation
}:
mkDerivation {
  pname = "hpython";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors bytestring containers deriving-compat digit dlist
    fingertree lens megaparsec mtl parsers parsers-megaparsec
    semigroupoids text these validation
  ];
  executableHaskellDepends = [ base lens text ];
  testHaskellDepends = [
    base digit directory filepath hedgehog lens megaparsec mtl process
    semigroupoids text these transformers validation
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq megaparsec text
  ];
  license = stdenv.lib.licenses.bsd3;
}
