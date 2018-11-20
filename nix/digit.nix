{ mkDerivation, ansi-wl-pprint, base, fetchgit, hedgehog, lens
, parsec, parsers, pretty, scientific, semigroupoids, semigroups
, stdenv, tasty, tasty-hedgehog, tasty-hspec, tasty-hunit
, template-haskell, text
}:
mkDerivation {
  pname = "digit";
  version = "0.7";
  src = fetchgit {
    url = "https://github.com/qfpl/digit";
    sha256 = "1hm181p7g7547887rjcp47l6rjf9xc059124pl90cw1fd90h5bm8";
    rev = "82adc046c050cd8e633b3b4d7b0fcb734465e0b5";
  };
  libraryHaskellDepends = [
    base lens parsers scientific semigroupoids semigroups
    template-haskell
  ];
  testHaskellDepends = [
    ansi-wl-pprint base hedgehog lens parsec parsers pretty semigroups
    tasty tasty-hedgehog tasty-hspec tasty-hunit text
  ];
  homepage = "https://github.com/qfpl/digit";
  description = "A data-type representing digits 0-9 and other combinations";
  license = stdenv.lib.licenses.bsd3;
}
