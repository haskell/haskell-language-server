# Features

- Code evaluation codelens ([Tutorial](https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-eval-plugin/README.md)):

   ![Eval Demo](https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-eval-plugin/demo.gif)

- Type information and documentation on hover. Note that currently, in order for docs to be displayed for dependencies, they must have been built with GHC's `-haddock` flag:

  - For cabal:
      - Add to your global config file (e.g. `~/.cabal/config`):

        ```
        program-default-options
          ghc-options: -haddock
        ```

      - Or, for a single project, run `cabal configure --ghc-options=-haddock`

  - For stack, add to global `$STACK_ROOT\config.yaml`, or project's `stack.yaml`:

    ```
    ghc-options:
      "$everything": -haddock
    ```

  This will cause compilation errors if a dependency contains invalid Haddock markup, though from GHC version 9.0, [these will be demoted to warnings](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2377).

 - Integration with [retrie](https://hackage.haskell.org/package/retrie)

   ![Retrie](https://i.imgur.com/Ev7B87k.gif)

 - Code lenses for explicit import lists

   ![Imports code lens](https://imgur.com/pX9kvY4.gif)

 - Many more (TBD)
