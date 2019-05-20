{ mkDerivation, base, extra, hspec, QuickCheck, rio, say, split
, stdenv
}:
mkDerivation {
  pname = "ticket-reservation";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base extra rio say split ];
  executableHaskellDepends = [ base extra rio say split ];
  testHaskellDepends = [ base extra hspec QuickCheck rio say split ];
  description = "Toy project implementation of a theater ticket reservation system using CLI";
  license = stdenv.lib.licenses.mit;
}
