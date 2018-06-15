{ mkDerivation, base, bytestring-trie, containers, deriving-compat
, directory, filepath, fingertree, hedgehog, lens, mtl, parsers
, process, semigroupoids, stdenv, transformers, trifecta
, type-level-sets
}:
mkDerivation {
  pname = "hpython";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring-trie containers deriving-compat fingertree lens mtl
    parsers semigroupoids trifecta type-level-sets
  ];
  executableHaskellDepends = [ base lens ];
  testHaskellDepends = [
    base directory filepath hedgehog lens mtl process semigroupoids
    transformers trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
