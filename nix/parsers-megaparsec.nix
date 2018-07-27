{ mkDerivation, base, fetchgit, megaparsec, mtl, parsers, stdenv
, text
}:
mkDerivation {
  pname = "parsers-megaparsec";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/qfpl/parsers-megaparsec";
    sha256 = "0c7i3z9mmpj46856im99g4fh6jkqhf16pf8x9wfxb5v40dm1flcx";
    rev = "f449676caa64293535ea155f9f3b86f83eefe332";
  };
  libraryHaskellDepends = [ base megaparsec mtl parsers text ];
  description = "`parsers` instances for Megaparsec";
  license = stdenv.lib.licenses.bsd3;
}
