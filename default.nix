{ mkDerivation, base, megaparsec, parsec, stdenv }:
mkDerivation {
  pname = "hamler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base parsec ];
  executableHaskellDepends = [ base megaparsec ];
  homepage = "https://github.com/hamler-lang/hamler";
  license = stdenv.lib.licenses.bsd3;
}
