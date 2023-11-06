{
  description = "Advent of code 2023";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskell-packages = nixpkgs.legacyPackages.${system}.haskell.packages;
        ghcVersion = "ghc946";
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        packages = {
          default = haskell-packages.${ghcVersion}.developPackage
            {
              root = ./.;
            };
        };
        devShells = {
          default =
            pkgs.mkShell {
              nativeBuildInputs = [
                pkgs.blas
                pkgs.ghc
                pkgs.haskell-language-server
                pkgs.haskellPackages.cabal-install
                pkgs.haskellPackages.fourmolu
                pkgs.haskellPackages.ghcid
                pkgs.lapack
                pkgs.zlib
              ];
            };
        };
      });
}
