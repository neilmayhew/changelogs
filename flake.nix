{
  description = "Utilities for processing cardano-ledger changelogs";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with
        import nixpkgs { inherit system; };
      let
        packages = lib.attrsets.mapAttrs (_: compiler: compiler.callPackage ./. { }) haskell.packages;
        supported = { inherit (packages) ghc8107 ghc928 ghc948 ghc967 ghc984 ghc9102 ghc9122; };
        default = packages.ghc967;
      in
      {
        packages = packages // {
          inherit default;
          ci = linkFarm "ci" supported;
        };
        devShells = lib.attrsets.mapAttrs (_: p: p.env) self.packages.${system};
      }
    );
}
