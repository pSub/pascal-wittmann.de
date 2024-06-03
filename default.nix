{ compiler ? "ghc96", nixpkgs ? import <nixpkgs> {} }:
let
  # Use my fork as long as github.com/bobjflong/yesod-csp/issues/8 is
  # not fixed.
  yesod-csp = (
    import (
      nixpkgs.fetchFromGitHub {
        owner = "pSub";
        repo = "yesod-csp";
        rev = "011d52d1f4a8415ae486f3c38f3935c778a3878d";
        hash = "sha256-4I9toFJBvw1rzVCAG/qJBJwT2I946SvL9r8aNLN5eAA=";
      }
    )
  ) { inherit nixpkgs; };

in

nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./homepage.nix { inherit yesod-csp; }
