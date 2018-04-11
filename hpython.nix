{ mkDerivation, base, bytestring-trie, directory, filepath
, hedgehog, lens, mtl, parsers, process, stdenv, these
, transformers, trifecta, type-level-sets
}:
mkDerivation {
  pname = "hpython";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring-trie lens mtl parsers these trifecta
    type-level-sets
  ];
  executableHaskellDepends = [ base lens ];
  testHaskellDepends = [
    base directory filepath hedgehog lens mtl process transformers
    trifecta
  ];
  license = stdenv.lib.licenses.bsd3;
}
