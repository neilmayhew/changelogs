{
  description = "Utilities for processing cardano-ledger changelogs";

  inputs = { flake-utils.url = "github:numtide/flake-utils"; };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      with
        import nixpkgs { inherit system; };
      let
        packages = lib.attrsets.mapAttrs (_: compiler: compiler.callPackage ./. { }) haskell.packages;
      in
      {
        packages = packages // { default = packages.ghc910; };
        devShells = lib.attrsets.mapAttrs (_: p: p.env) self.packages.${system};
      }
    );
}
