{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      haskellDeps = drv: builtins.concatLists (builtins.attrValues drv.getCabalDeps);
      hello-haskell = pkgs.haskellPackages.callCabal2Nix "hello-haskell" ./. { };
    in
    {
      packages.default = hello-haskell;

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = with pkgs.haskellPackages; [
          (ghcWithPackages (ps: haskellDeps hello-haskell))
          cabal-install
          haskell-language-server
        ];
      };
    });
}
