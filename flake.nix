{
  description = "Assembler for the XJ24 microprocessor";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs { inherit system; });
        hpkgs = pkgs.haskellPackages;

        my-drv = { root = ./.; };
      in
      {
        defaultPackage = hpkgs.developPackage my-drv;

        devShell = hpkgs.developPackage (my-drv // {
          returnShellEnv = true;
          withHoogle = false;
          modifier = drv: pkgs.haskell.lib.addBuildTools drv (
            with pkgs;
            with hpkgs;
            [
              bashInteractive
              cabal-install
              haskell-language-server
              hpack
            ]
          );
        });
      });
}
