{ mkDerivation, base, bifunctors, deepseq, fetchgit, hedgehog
, HUnit, lens, semigroupoids, semigroups, stdenv
}:
mkDerivation {
  pname = "validation";
  version = "1";
  src = fetchgit {
    url = "https://github.com/qfpl/validation";
    sha256 = "1hkd7ma38rdq9rw0bbivi3l33i01idy5qka2x9fmzvfkjzr17q1q";
    rev = "b7c5c0cf7d0bdbd8c0c47ac03d3a83bc75fc5f57";
  };
  libraryHaskellDepends = [
    base bifunctors deepseq lens semigroupoids semigroups
  ];
  testHaskellDepends = [ base hedgehog HUnit lens semigroups ];
  homepage = "https://github.com/qfpl/validation";
  description = "A data-type like Either but with an accumulating Applicative";
  license = stdenv.lib.licenses.bsd3;
}
