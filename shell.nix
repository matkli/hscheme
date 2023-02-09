let
  nixpkgs = import <nixpkgs> {};
  ghc = nixpkgs.haskellPackages.ghcWithPackages (pkgs: [ pkgs.cabal-install ] );
in nixpkgs.mkShell {
  packages = [ ghc ];
}
