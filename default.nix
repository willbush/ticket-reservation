{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "ticket-reservation";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  description = "Toy project implementation of a theater ticket reservation system using CLI";
  license = stdenv.lib.licenses.mit;
}
