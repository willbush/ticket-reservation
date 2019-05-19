{ mkDerivation, base, hspec, QuickCheck, rio, say, stdenv }:
mkDerivation {
  pname = "ticket-reservation";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base rio say ];
  executableHaskellDepends = [ base rio say ];
  testHaskellDepends = [ base hspec QuickCheck rio say ];
  description = "Toy project implementation of a theater ticket reservation system using CLI";
  license = stdenv.lib.licenses.mit;
}
