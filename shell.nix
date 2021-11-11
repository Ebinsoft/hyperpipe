{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.shellFor {
  packages = p: [
    (import ./default.nix {})
  ];
  buildInputs = [
    pkgs.cabal-install
  ];
  withHoogle = true;
}
