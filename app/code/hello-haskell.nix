{ pkgs, ... }:

pkgs.haskellPackages.callCabal2Nix "hello-haskell" ./. { }
