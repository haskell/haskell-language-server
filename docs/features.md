# Features

- Warning and error diagnostics from GHC
- Type information and documentation on hover, [including your own comments](#how-to-show-local-documentation-on-hover).
- Jump to definition: [for now only for local code definitions](https://github.com/haskell/haskell-language-server/issues/708)
- Document symbols
- Highlight references in document
- Code completion
- Formatting via Brittany, Floskell, Fourmolu, Ormolu or Stylish Haskell
- Code evaluation (Haskell Language Server), see ([Tutorial](https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin/README.md))

  ![Eval Demo](https://raw.githubusercontent.com/haskell/haskell-language-server/master/plugins/hls-eval-plugin/demo.gif)

- Integration with [retrie](https://hackage.haskell.org/package/retrie)

  ![Retrie Demo](https://i.imgur.com/Ev7B87k.gif)

- Code lenses for explicit import lists

  ![Imports code lens Demo](https://imgur.com/pX9kvY4.gif)

- Generate functions from type signatures, and intelligently complete holes using [Wingman (tactics)](https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-tactics-plugin)

  ![Wingman Demo](https://user-images.githubusercontent.com/307223/92657198-3d4be400-f2a9-11ea-8ad3-f541c8eea891.gif)

- Integration with [hlint](https://github.com/ndmitchell/hlint) to show diagnostics and apply hints via [apply-refact](https://github.com/mpickering/apply-refact)

  ![Hlint Demo](https://user-images.githubusercontent.com/54035/110860028-8f9fa900-82bc-11eb-9fe5-6483d8bb95e6.gif)

- Module name suggestions for insertion or correction

  ![Module Name Demo](https://user-images.githubusercontent.com/54035/110860755-78ad8680-82bd-11eb-9845-9ea4b1cc1f76.gif)

- Call hierarchy support

  ![Call Hierarchy in VSCode](https://github.com/haskell/haskell-language-server/raw/2857eeece0398e1cd4b2ffb6069b05c4d2308b39/plugins/hls-call-hierarchy-plugin/call-hierarchy-in-vscode.gif)
