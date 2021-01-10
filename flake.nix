{
  description = "Receipt capture";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignoreSrc = {
    url = "github:hercules-ci/gitignore.nix";
    flake = false;
  };

  outputs = { self, nixpkgs, nur, flake-utils, gitignoreSrc }: 
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          gitignoreSource = (import gitignoreSrc { inherit lib; }).gitignoreSource;
          app = pkgs.callPackage ./app.nix { inherit pkgs gitignoreSource; };
        in
          rec {
            packages = flake-utils.lib.flattenTree { receipt-capture = app.base; };
            defaultPackage = packages.receipt-capture;
            apps.receipt-capture = flake-utils.lib.mkApp { drv = packages.receipt-capture; };
            defaultApp = apps.receipt-capture;
            devShell = app.shell;
          }
    );
}
