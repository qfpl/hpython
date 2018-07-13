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
    sha256 = "0lxsy5ydj2gir06pwd0qb2fdhnbr5pfng2rqix4lgck562rar782";
    rev = "d03309afac13fbed6814944386d7298c4545d7a2";
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
