# `ghcide` - A library for building Haskell IDE tooling

## Using it

`ghcide` is not an end-user tool, [don't use `ghcide`](https://neilmitchell.blogspot.com/2020/09/dont-use-ghcide-anymore-directly.html) directly (more about the rationale [here](https://github.com/haskell/ghcide/pull/939)).

 [`haskell-language-server`](http://github.com/haskell/haskell-language-server) is an LSP server built on top of `ghcide` with additional features and a user friendly deployment model. To get it, simply install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) in VS Code, or download prebuilt binaries from the [haskell-language-server](https://github.com/haskell/haskell-language-server) project page.


The instructions below are meant for developers interested in setting up ghcide as an LSP server for testing purposes.

### Install `ghcide`

#### With Nix

Note that you need to compile `ghcide` with the same `ghc` as the project you are working on.

1. If the `ghc` you are using matches the version (or better is) from `nixpkgs` it‘s easiest to use the `ghcide` from `nixpkgs`. You can do so via
   ```
   nix-env -iA haskellPackages.ghcide
   ```
   or e.g. including `pkgs.haskellPackages.ghcide` in your projects `shell.nix`.
   Depending on your `nixpkgs` channel that might not be the newest `ghcide`, though.

2. If your `ghc` does not match nixpkgs you should try the [ghcide-nix repository](https://github.com/cachix/ghcide-nix)
   which provides a `ghcide` via the `haskell.nix` infrastructure.

#### With Cabal or Stack

First install the `ghcide` binary using `stack` or `cabal`, e.g.

1. `git clone https://github.com/haskell/ghcide.git`
2. `cd ghcide`
3. `cabal install` or `stack install` (and make sure `~/.local/bin` is on your `$PATH`)

It's important that `ghcide` is compiled with the same compiler you use to build your projects.
