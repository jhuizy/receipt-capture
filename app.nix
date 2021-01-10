{ pkgs, gitignoreSource }:
let
  base = pkgs.haskellPackages.callCabal2nix "receipt-capture" ./. { };
  project = base.env.overrideAttrs (
    old: with pkgs; {
      nativeBuildInputs = old.nativeBuildInputs ++ [
        coreutils
        tesseract4
      ];
    }
  );
  shell = pkgs.mkShell {
    buildInputs = with pkgs; project.nativeBuildInputs ++ [
      haskellPackages.cabal-install
      haskellPackages.ghcide
      haskellPackages.cabal2nix
      haskellPackages.hlint
      haskellPackages.brittany
      haskellPackages.cabal2nix
      haskellPackages.ghcid
      gitAndTools.pre-commit
    ];
    shellHook = ''
      export NIX_GHC="`which ghc`"
      export NIX_GHCPKG="`which ghc`/../ghc-pkg"
      export NIX_GHC_DOCDIR="`which ghc`/../../share/doc/ghc/html"
      export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
    '';
  };
in
{
  base = base;
  project = project;
  shell = shell;
}
