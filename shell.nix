let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  gitignoreSource = (import sources."gitignore.nix" { inherit (pkgs) lib;}).gitignoreSource;
  project = pkgs.haskellPackages.callCabal2nix "receipt-capture" (gitignoreSource ./.) { };
in
pkgs.mkShell {
  buildInputs = with pkgs; project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskellPackages.ghcide
    haskellPackages.cabal2nix
    haskellPackages.hlint
    haskellPackages.brittany
    haskellPackages.cabal2nix
    haskellPackages.ghcid
    coreutils
    gitAndTools.pre-commit
    tesseract4
  ];
  shellHook = ''
    export NIX_GHC="`which ghc`"
    export NIX_GHCPKG="`which ghc`/../ghc-pkg"
    export NIX_GHC_DOCDIR="`which ghc`/../../share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
