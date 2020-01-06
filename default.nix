{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "hamler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.asl20;
}
