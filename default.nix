{ mkDerivation, array, base, fail, ghc-prim, happy, pretty
, semigroups, stdenv
}:
mkDerivation {
  pname = "hamler";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base fail ghc-prim pretty semigroups
  ];
  libraryToolDepends = [ happy ];
  executableHaskellDepends = [ array base ];
  homepage = "https://github.com/hamler-lang/hamler";
  description = "The Hamler Programming Language";
  license = stdenv.lib.licenses.bsd3;
}
