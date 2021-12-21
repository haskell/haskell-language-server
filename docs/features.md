# Features

You can watch demos for some of these features [below](#demos).

- Warning and error diagnostics from GHC
- Type information and documentation on hover, [including your own comments](./configuration.md#how-to-show-local-documentation-on-hover).
- Jump to definition: [for now only for local code definitions](https://github.com/haskell/haskell-language-server/issues/708)
- Document symbols
- Highlight references in document
- Code completion
- Formatting via [Brittany](https://github.com/lspitzner/brittany), [Floskell](https://github.com/ennocramer/floskell), [Fourmolu](https://github.com/fourmolu/fourmolu), [Ormolu](https://github.com/tweag/ormolu) or [Stylish Haskell](https://github.com/haskell/stylish-haskell)
- [Code evaluation](#code-evaluation), see its [Tutorial](https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin/README.md)
- [Integration with](#retrie-integration) [retrie](https://hackage.haskell.org/package/retrie), a powerful, easy-to-use codemodding tool
- [Code lenses for explicit import lists](#explicit-import-lists)
- [Generate functions from type signatures, and intelligently complete holes](#wingman) using [Wingman (tactics)](https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-tactics-plugin)
- [Integration](#hlint) with [hlint](https://github.com/ndmitchell/hlint), the most used haskell linter, to show diagnostics and apply hints via [apply-refact](https://github.com/mpickering/apply-refact)
- [Module name suggestions](#module-names) for insertion or correction
- [Call hierarchy support](#call-hierarchy)
- [Qualify names from an import declaration](#qualify-imported-names) in your code
- [Suggest alternate numeric formats](#alternate-number-formatting). This plugin is not included by default yet due to a performance issue, see <https://github.com/haskell/haskell-language-server/issues/2490>

## Demos

### Code evaluation

![Eval Demo](https://raw.githubusercontent.com/haskell/haskell-language-server/master/plugins/hls-eval-plugin/demo.gif)

### Retrie integration

![Retrie Demo](https://i.imgur.com/Ev7B87k.gif)

### Explicit import lists

![Imports code lens Demo](https://imgur.com/pX9kvY4.gif)

### Wingman

![Wingman Demo](https://user-images.githubusercontent.com/307223/92657198-3d4be400-f2a9-11ea-8ad3-f541c8eea891.gif)

### Hlint

![Hlint Demo](https://user-images.githubusercontent.com/54035/110860028-8f9fa900-82bc-11eb-9fe5-6483d8bb95e6.gif)

### Module names

![Module Name Demo](https://user-images.githubusercontent.com/54035/110860755-78ad8680-82bd-11eb-9845-9ea4b1cc1f76.gif)

### Call hierarchy

![Call Hierarchy in VSCode](https://github.com/haskell/haskell-language-server/raw/2857eeece0398e1cd4b2ffb6069b05c4d2308b39/plugins/hls-call-hierarchy-plugin/call-hierarchy-in-vscode.gif)

### Qualify imported names

![Qualify Imported Names Demo](../plugins/hls-qualify-imported-names-plugin/qualify-imported-names-demo.gif)

### Alternate Number Formatting

![Alternate Number Format Demo](../plugins/hls-alternate-number-format-plugin/HLSAll.gif)
