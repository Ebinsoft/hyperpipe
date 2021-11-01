#!/usr/bin/env nix-shell
{
  pkgs ? import <nixpkgs> {}
}:
  with pkgs;
  haskell.lib.buildStackProject {
    name = "hyperpipe";
    buildInputs = [ 
      pkgs.cabal-install 
      pkgs.haskellPackages.hoogle 
    ];
    shellHook = ''
      echo 'Entering Environment'
      alias stack='\stack --nix'
    '';
  }
