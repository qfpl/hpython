{ mkDerivation, base, fetchgit, ghc-prim, stdenv }:
mkDerivation {
  pname = "type-level-sets";
  version = "0.8.7.0";
  src = fetchgit {
    url = "https://github.com/dorchard/type-level-sets";
    sha256 = "1w0dyjs1sdkm8gb9h664c1kx1xy3r4nzgd1dwiwyzsvy7snghrnc";
    rev = "850dd32b42ab4915ecd1c78c6b7cdf17f2f40f59";
  };
  libraryHaskellDepends = [ base ghc-prim ];
  description = "Type-level sets and finite maps (with value-level counterparts)";
  license = stdenv.lib.licenses.bsd3;
}
