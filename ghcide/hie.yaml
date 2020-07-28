cradle:
  multi:
    - path: "./test/data"
      config: { cradle: { none:  } }
    - path: "./"
      config:
        cradle:
          cabal:
            - path: "./src"
              component: "ghcide:lib:ghcide"
            - path: "./exe"
              component: "ghcide:exe:ghcide"
            - path: "./session-loader"
              component: "ghcide:lib:ghcide"
            - path: "./test"
              component: "ghcide:test:ghcide-tests"
            - path: "./bench"
              component: "ghcide:exe:ghcide-bench"
            - path: "./bench/hist"
              component: "ghcide:bench:benchHist"
            - path: "./test/preprocessor"
              component: "ghcide:exe:ghcide-test-preprocessor"
