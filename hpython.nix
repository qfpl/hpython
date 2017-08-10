{ mkDerivation, ansi-wl-pprint, base, charset, deriving-compat
, hedgehog, hspec, papa, parsers, pretty, stdenv, tasty
, tasty-hspec, text, transformers, trifecta
}:
mkDerivation {
  pname = "hpython";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base charset deriving-compat papa parsers pretty text transformers
    trifecta
  ];
  testHaskellDepends = [
    ansi-wl-pprint base hedgehog hspec papa pretty tasty tasty-hspec
    trifecta
  ];
  homepage = "https://github.com/qfpl/hpython";
  description = "Write Python using Haskell";
  license = stdenv.lib.licenses.bsd3;
}
