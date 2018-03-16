{ mkDerivation, base, hedgehog, lens, mtl, parsers, process, stdenv
, transformers, trifecta, type-level-sets
}:
mkDerivation {
  pname = "hpython";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base lens mtl parsers trifecta type-level-sets
  ];
  executableHaskellDepends = [ base lens ];
  testHaskellDepends = [ base hedgehog lens process transformers ];
  license = stdenv.lib.licenses.bsd3;
}
