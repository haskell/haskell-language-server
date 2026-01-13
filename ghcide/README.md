# `ghcide` - A library for building Haskell IDE tooling

## Using it

`ghcide` is not an end-user tool, [don't use `ghcide`](https://neilmitchell.blogspot.com/2020/09/dont-use-ghcide-anymore-directly.html) directly (more about the rationale [here](https://github.com/haskell/ghcide/pull/939)).

 [`haskell-language-server`](http://github.com/haskell/haskell-language-server) is an LSP server built on top of `ghcide` with additional features and a user friendly deployment model. To get it, simply install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) in VS Code, or download prebuilt binaries from the [haskell-language-server](https://github.com/haskell/haskell-language-server) project page.


Set-up and usage instructions can be found on [haskell-language-server documentation](https://haskell-language-server.readthedocs.io/en/latest/components/ghcide.html)
