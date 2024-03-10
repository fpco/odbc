# -*- compile-command: "nix-shell --run 'cabal exec -- ghc-pkg list'"; -*-
(import ./. {}).env
