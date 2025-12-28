{
  description = "Homepage pascal-wittmann.de";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    yesod-csp-nix.url = "github:pSub/yesod-csp";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      yesod-csp-nix
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.homepage = nixpkgs.pkgs.haskell.packages.ghc910.callPackage ./homepage.nix {
          yesod-csp = yesod-csp-nix.packages.${system}.yesod-csp;
        };
      }
    );
}
