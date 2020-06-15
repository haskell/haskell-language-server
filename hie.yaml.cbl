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
            - path: "./bench"
              component: "ghcide:bench:ghcide-bench"
            - path: "./bench/Hist"
              component: "ghcide:exe:benchHist"
            - path: "./test"
              component: "ghcide:test:ghcide-tests"
            - path: "./test/preprocessor"
              component: "ghcide:exe:ghcide-test-preprocessor"
