{ mkDerivation, ansi-wl-pprint, base, charset, containers
, deriving-compat, digit, directory, dlist, filepath, hedgehog
, indentation-trifecta, mtl, papa, parsers, pretty, process
, singletons, stdenv, tasty, tasty-hedgehog, tasty-hspec, text
, transformers, trifecta, unordered-containers, validation
}:
mkDerivation {
  pname = "hpython";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base charset containers deriving-compat digit dlist
    indentation-trifecta mtl papa parsers pretty singletons text
    transformers trifecta unordered-containers validation
  ];
  testHaskellDepends = [
    ansi-wl-pprint base directory filepath hedgehog papa pretty process
    tasty tasty-hedgehog tasty-hspec text transformers trifecta
  ];
  homepage = "https://github.com/qfpl/hpython";
  description = "Write Python using Haskell";
  license = stdenv.lib.licenses.bsd3;
}
