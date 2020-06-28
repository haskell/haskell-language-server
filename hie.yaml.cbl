# This is a sample hie.yaml file for opening haskell-language-server
# in hie, using cabal as the build system.  To use is, copy it to a
# file called 'hie.yaml'
cradle:
  multi:
    - path: "./test/testdata/"
      config: { cradle: { none:  } }

    - path: ./Setup.hs
      config:
        cradle:
          direct:
            arguments:
              - "-package Cabal"
              - "-package base"

    - path: "./"
      config:
        cradle:
          cabal:
            - path: "./test/functional/"
              component: "haskell-language-server:func-test"

            - path: "./test/utils/"
              component: "haskell-language-server:func-test"
        
            - path: "./exe/Main.hs"
              component: "haskell-language-server:exe:haskell-language-server"
        
            - path: "./exe/Arguments.hs"
              component: "haskell-language-server:exe:haskell-language-server"

            - path: "./exe/Wrapper.hs"
              component: "haskell-language-server:exe:haskell-language-server-wrapper"
        
            - path: "./src"
              component: "lib:haskell-language-server"

            - path: "./.stack-work/"
              component: "lib:haskell-language-server"
        
            - path: "./ghcide/src"
              component: "ghcide:lib:ghcide"
        
            - path: "./ghcide/exe"
              component: "ghcide:exe:ghcide"
