{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    reflex-arc = {
      url = "github:ners/reflex-arc/colour-schemes";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.flake-compat.follows = "flake-compat";
    };
    reflex-slides = {
      url = "github:ners/reflex-slides";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.flake-compat.follows = "flake-compat";
      inputs.reflex-arc.follows = "reflex-arc";
    };
    web-font-mdi = {
      url = "github:ners/web-font-mdi";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);
      reflex-arc = inputs.reflex-arc.packages.${system}.default;
      reflex-slides = inputs.reflex-slides.packages.${system}.default;
      haskell-primer-slides = haskellPackages.callCabal2nix "haskell-primer-slides" ./. {
        inherit reflex-arc reflex-slides;
        inherit (inputs.web-font-mdi.packages.${system}) web-font-mdi;
      };
    in
    {
      packages = {
        inherit haskell-primer-slides;
        default = haskell-primer-slides;
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          (haskellPackages.ghcWithPackages (ps: haskellDeps haskell-primer-slides))
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hpack
        ];
      };
    });
}
