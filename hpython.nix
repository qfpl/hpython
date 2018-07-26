{ mkDerivation, base, bytestring-trie, containers, criterion
, deepseq, deriving-compat, digit, directory, dlist, filepath
, fingertree, hedgehog, lens, mtl, parsers, process, semigroupoids
, stdenv, these, transformers, trifecta, type-level-sets
}:
mkDerivation {
  pname = "hpython";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring-trie containers deriving-compat digit dlist
    fingertree lens mtl parsers semigroupoids these trifecta
    type-level-sets
  ];
  executableHaskellDepends = [
    base criterion deepseq lens trifecta
  ];
  testHaskellDepends = [
    base digit directory filepath hedgehog lens mtl process
    semigroupoids these transformers trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
