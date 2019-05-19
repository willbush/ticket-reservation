let
  pinnedPkgs = import ./nix/pkgs-from-json.nix { json = ./nix/nixos-19-03.json; };
  myPackages = (import ./nix/release.nix { withHoogle = true; } );

  projectDrvEnv = myPackages.project1.env.overrideAttrs (oldAttrs: rec {
    buildInputs = oldAttrs.buildInputs ++ [
      pinnedPkgs.haskellPackages.cabal-install
    ];
  });
in
  projectDrvEnv
