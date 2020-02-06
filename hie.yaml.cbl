# This is a sample hie.yaml file for opening haskell-language-server
# in hie, using cabal as the build system.  To use is, copy it to a
# file called 'hie.yaml'
# WARNING: This configuration works for hie but does not for
#          haskell-language-server or ghcide.
#          They need support for multi-cradle:
#          https://github.com/digital-asset/ghcide/issues/113
cradle:
  cabal:

    - path: "./test"
      component: "haskell-language-server:hls-tests"

    - path: "./test/utils/"
      component: "haskell-language-server:hls-test-utils"

    - path: "./exe/Main.hs"
      component: "haskell-language-server:exe:haskell-language-server"

    - path: "./exe/Wrapper.hs"
      component: "haskell-language-server:exe:haskell-language-server-wrapper"

    - path: "./src"
      component: "lib:haskell-language-server"

    - path: "./ghcide/src"
      component: "ghcide:lib:ghcide"

    - path: "./ghcide/exe"
      component: "ghcide:exe:ghcide"
