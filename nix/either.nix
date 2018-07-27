{ mkDerivation, base, bifunctors, mtl, profunctors, QuickCheck
, semigroupoids, semigroups, stdenv, test-framework
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "either";
  version = "5.0.1";
  sha256 = "6cb6eb3f60223f5ffedfcd749589e870a81d272e130cafd1d17fb6d3a8939018";
  libraryHaskellDepends = [
    base bifunctors mtl profunctors semigroupoids semigroups
  ];
  testHaskellDepends = [
    base QuickCheck test-framework test-framework-quickcheck2
  ];
  homepage = "http://github.com/ekmett/either/";
  description = "Combinators for working with sums";
  license = stdenv.lib.licenses.bsd3;
}
