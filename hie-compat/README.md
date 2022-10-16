# hie-compat

Mainly a backport of [HIE
Files](https://gitlab.haskell.org/ghc/ghc/-/wikis/hie-files) for ghc 8.8, along
with a few other backports of fixes useful for `ghcide`

Also includes backport of record-dot-syntax support to 9.2.x

Fully compatible with `.hie` files natively produced by versions of GHC that support
them.

**THIS DOES NOT LET YOU READ HIE FILES WITH MISMATCHED VERSIONS OF GHC**

Backports included:

https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8589

https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4037

https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4068

https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3199

https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2578
