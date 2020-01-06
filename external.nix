{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      hamler = self.callPackage ./default.nix {};
    };
  };

  haskellDeps = ps: with ps; [
    base
    hamler
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    haskellPackages.hamler
    haskellPackages.cabal-install
  ];
in
pkgs.stdenv.mkDerivation {
  name = "env";
  buildInputs = nixPackages;
}
