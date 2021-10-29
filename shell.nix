#!/usr/bin/env nix-shell
{
  pkgs ? import <nixpkgs> {}
}:
  with pkgs;
  haskell.lib.buildStackProject {
    name = "haskell-template";
    buildInputs = [ pkgs.cabal-install ];
    shellHook = ''
      echo 'Entering Environment'
      alias stack='\stack --nix'
    '';
  }
