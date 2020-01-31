# This is a sample hie.yaml file for opening haskell-ide-engine in
# hie, using cabal as the build system.
# To use is, copy it to a file called 'hie.yaml'

cradle:
  cabal:

    - path: "./test"
      component: "ide:test"

    - path: "./exe/Main.hs"
      component: "ide:exe:haskell-ide"

    - path: "./exe/Wrapper.hs"
      component: "ide:exe:haskell-ide-wrapper"

    - path: "./src"
      component: "lib:ide"

    - path: "./ghcide/src"
      component: "ghcide:lib:ghcide"

    - path: "./ghcide/exe"
      component: "ghcide:exe:ghcide"
