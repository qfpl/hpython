{ mkDerivation, ansi-wl-pprint, base, fetchgit, hedgehog, lens
, papa, parsec, parsers, pretty, scientific, semigroupoids
, semigroups, stdenv, tasty, tasty-hedgehog, tasty-hspec
, tasty-hunit, template-haskell, text
}:
mkDerivation {
  pname = "digit";
  version = "0.6";
  src = fetchgit {
    url = "https://github.com/qfpl/digit";
    sha256 = "0bm2bfg27ry7c7aspikvbqj9zk5f0iayy98yy5jk4sprhs9q7alv";
    rev = "27fd4bb3857649f16c5e5ba188cb1b03d436df60";
  };
  libraryHaskellDepends = [
    base lens papa parsers scientific semigroupoids semigroups
    template-haskell
  ];
  testHaskellDepends = [
    ansi-wl-pprint base hedgehog lens papa parsec parsers pretty tasty
    tasty-hedgehog tasty-hspec tasty-hunit text
  ];
  homepage = "https://github.com/qfpl/digit";
  description = "A data-type representing digits 0-9 and other combinations";
  license = stdenv.lib.licenses.bsd3;
}
