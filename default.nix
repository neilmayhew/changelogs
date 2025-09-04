{ mkDerivation, base, cmark, lib, mtl, optparse-applicative
, pretty-simple, terminal-size, text
}:
mkDerivation {
  pname = "changelogs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cmark mtl optparse-applicative pretty-simple terminal-size
    text
  ];
  description = "Utilities for processing cardano-ledger changelogs";
  license = lib.licenses.asl20;
  mainProgram = "changelogs";
}
