{ compiler, nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./homepage.nix { }
