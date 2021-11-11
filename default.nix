{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./hyperpipe.nix { }
