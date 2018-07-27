{ mkDerivation, base, fetchgit, megaparsec, mtl, parsers, stdenv
, text
}:
mkDerivation {
  pname = "parsers-megaparsec";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/qfpl/parsers-megaparsec";
    sha256 = "0y1xh8y85bp3n43fhaf0qa7kzyr90sgqzvc2rbrqy8ri5gjf5g06";
    rev = "1a96321f38d0a7b8e4331f63cba0383e6170cf4f";
  };
  libraryHaskellDepends = [ base megaparsec mtl parsers text ];
  description = "`parsers` instances for Megaparsec";
  license = stdenv.lib.licenses.bsd3;
}
