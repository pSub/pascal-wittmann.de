{
  compiler ? "ghc910",
  nixpkgs ? import <nixpkgs> { },
}:

let
  # Remove when the build is fixed on nixos-25.11
  yesod-csp =
    (import (
      nixpkgs.fetchFromGitHub {
        owner = "pSub";
        repo = "yesod-csp";
        rev = "f15a2dcdb9ae069360326895c00736333f564378";
        hash = "sha256-w4k45nXHX5zhLKDOw8NQ2aSM2pRzXQFb+9A/B5b65xo=";
      }
    ))
      { inherit nixpkgs; };

in
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./homepage.nix { inherit yesod-csp; }
