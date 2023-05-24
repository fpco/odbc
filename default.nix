# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
{ pkgs ? import <nixpkgs> {} }:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/make-package-set.nix
let this = pkgs.haskellPackages.developPackage {
    root = ./.;
    withHoogle = false;
    returnShellEnv = false;
    modifier = with pkgs.haskell.lib; drv:
      disableLibraryProfiling
        (addExtraLibraries
          (dontHaddock
            (addBuildTools drv
              (with pkgs.haskellPackages; [ cabal-install ghcid pkgs.unixODBC])))
        [ pkgs.unixODBC ]);

  };
in this
   // { env = this.env.overrideAttrs(_: prev: { shellHook = prev.shellHook + ''
   export LD_LIBRARY_PATH=${pkgs.unixODBC}/lib
   ''; });}
