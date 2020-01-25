# This is a sample hie.yaml file for opening haskell-ide-engine in
# hie, using cabal as the build system.
# To use is, copy it to a file called 'hie.yaml'

cradle:
  cabal:

    - path: "./test"
      component: "ide:test"

    - path: "./exe"
      component: "ide:exe:ide"

    - path: "./src"
      component: "lib:ide"
