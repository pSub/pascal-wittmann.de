{ pkgs ? (import <nixpkgs> {}) }:

(import ./default.nix) {
    stdenv            = pkgs.stdenv;
    haskellngPackages = pkgs.haskellngPackages;
  }