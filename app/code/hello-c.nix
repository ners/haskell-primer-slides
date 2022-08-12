{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "hello-c";
  version = "0.0.1";
  src = ./.;
}
