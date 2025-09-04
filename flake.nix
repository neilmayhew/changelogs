{
  description = "Utilities for processing cardano-ledger changelogs";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with
        import nixpkgs { inherit system; };
      let
        dynamic = pkgs;
        static = pkgsStatic;
        packages = {
          dynamic = lib.attrsets.mapAttrs
            (_: compiler:
              dynamic.haskell.lib.justStaticExecutables
                (compiler.callPackage ./. { }))
            dynamic.haskell.packages;
          static = lib.attrsets.mapAttrs
            (_: compiler:
              static.haskell.lib.justStaticExecutables
                (compiler.callPackage ./. { }))
            static.haskell.packages;
        };
      in
      {
        packages = packages // {
          default = packages.dynamic.ghc967;
          supported = linkFarm "supported" {
            inherit (packages.dynamic)
              ghc8107 ghc928 ghc948 ghc967 ghc984 ghc9102 ghc9122;
          };
        };
        devShells = lib.attrsets.mapAttrsRecursiveCond
          (as: !(as ? "type") || as.type != "derivation")
          (_: p: if p ? "env" then p.env else p)
          self.packages.${system};
      }
    );

  nixConfig = {
    extra-substituters = [
      "https://neil-mayhew.cachix.org"
    ];
    extra-trusted-public-keys = [
      "neil-mayhew.cachix.org-1:mxrzBmebKDFyT7RzZom+8uhFochoTk6BL/1UTBU64eY="
    ];
  };
}
