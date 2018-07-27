{ mkDerivation, base, fetchgit, megaparsec, mtl, parsers, stdenv
, text
}:
mkDerivation {
  pname = "parsers-megaparsec";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/qfpl/parsers-megaparsec";
    sha256 = "1aqs42va7b5sghkd44sb59d9m6g5rzjdg65rmg6r694qg1bqx7js";
    rev = "fddf5edf9cd579e5dda2d25eafab78ae31c09354";
  };
  libraryHaskellDepends = [ base megaparsec mtl parsers text ];
  description = "`parsers` instances for Megaparsec";
  license = stdenv.lib.licenses.bsd3;
}
