{ nixpkgs ? import <nixpkgs> {} }:
(import ./default.nix { compiler = "ghc7103"; inherit nixpkgs; }).env
