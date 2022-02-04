{ compiler ? "ghc8107", nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./homepage.nix { }
