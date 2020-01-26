# ide

Integration point for ghcide and haskell-ide-engine. One IDE to rule them all.

This is *very* early stage software.

To play along at home, do a recursive `git clone`.

Initial effort is to understand how plugins can be supported in a modular way.

Builds with stack and cabal, using GHC 8.6.5

Two sample `hie.yaml` files are provided, `hie.yaml.stack` for stack
usage, `hie.yaml.cbl` for cabal. Simply copy the relevant one to be
`hie.yaml` and it should work.
