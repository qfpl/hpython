{ mkDerivation, base, bytestring-trie, directory, hedgehog, lens
, mtl, parsers, process, stdenv, transformers, trifecta
, type-level-sets
}:
mkDerivation {
  pname = "hpython";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring-trie lens mtl parsers trifecta type-level-sets
  ];
  executableHaskellDepends = [ base lens ];
  testHaskellDepends = [
    base directory hedgehog lens mtl process transformers trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
