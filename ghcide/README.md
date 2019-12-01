# `ghcide` - A library for building Haskell IDE tooling

Note: `ghcide` was previously called `hie-core`.

Our vision is that you should build an IDE by combining:

![vscode](https://raw.githubusercontent.com/digital-asset/ghcide/master/img/vscode2.png)

* [`hie-bios`](https://github.com/mpickering/hie-bios) for determining where your files are, what are their dependencies, what extensions are enabled and so on;
* `ghcide` (i.e. this library) for defining how to type check, when to type check, and producing diagnostic messages;
* A bunch of plugins that haven't yet been written, e.g. [`hie-hlint`](https://github.com/ndmitchell/hlint) and [`hie-ormolu`](https://github.com/tweag/ormolu), to choose which features you want;
* [`haskell-lsp`](https://github.com/alanz/haskell-lsp) for sending those messages to a [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) server;
* An extension for your editor. We provide a [VS Code extension](https://code.visualstudio.com/api) as `extension` in this directory, although the components work in other LSP editors too (see below for instructions using Emacs).

There are more details about our approach [in this blog post](https://4ta.uk/p/shaking-up-the-ide).

## Features

`ghcide` already exports the following features via the lsp protocol:

| Feature | LSP name |
| - | - |
| Display error messages (parse errors, typecheck errors, etc.) and enabled warnings. | diagnostics |
| Go to definition in local package | definition  |
| Display type and source module of values | hover |
| Remove redundant imports, replace suggested typos for values and module imports, fill type holes, insert missing type signatures, add suggested ghc extensions  | codeAction (quickfix) |

## Using it

### Install `ghcide`

#### With Nix

[See ghcide-nix repository](https://github.com/hercules-ci/ghcide-nix)

#### With Cabal or Stack

First install the `ghcide` binary using `stack` or `cabal`, e.g.

1. `git clone https://github.com/digital-asset/ghcide.git`
2. `cd ghcide`
3. `cabal install` or `stack install` (and make sure `~/.local/bin` is on your `$PATH`)

It's important that `ghcide` is compiled with the same compiler you use to build your projects.

### Test `ghcide`

Next, check that `ghcide` is capable of loading your code. Change to the project directory and run `ghcide`, which will try and load everything using the same code as the IDE, but in a way that's much easier to understand. For example, taking the example of [`shake`](https://github.com/ndmitchell/shake), running `ghcide` gives some error messages and warnings before reporting at the end:

```console
Files that failed:
 * .\model\Main.hs
 * .\model\Model.hs
 * .\model\Test.hs
 * .\model\Util.hs
 * .\output\docs\Main.hs
 * .\output\docs\Part_Architecture_md.hs
Completed (152 worked, 6 failed)
```

Of the 158 files in Shake, as of this moment, 152 can be loaded by the IDE, but 6 can't (error messages for the reasons they can't be loaded are given earlier). The failing files are all prototype work or test output, meaning I can confidently use Shake.

The `ghcide` executable mostly relies on [`hie-bios`](https://github.com/mpickering/hie-bios) to do the difficult work of setting up your GHC environment. If it doesn't work, see [the `hie-bios` manual](https://github.com/mpickering/hie-bios#readme) to get it working. My default fallback is to figure it out by hand and create a `direct` style [`hie.yaml`](https://github.com/ndmitchell/shake/blob/master/hie.yaml) listing the command line arguments to load the project.

If you can't get `ghcide` working outside the editor, see [this setup troubleshooting guide](docs/Setup.md). Once you have got `ghcide` working outside the editor, the next step is to pick which editor to integrate with.

### Using with VS Code

You can install the VSCode extension from the [VSCode
marketplace](https://marketplace.visualstudio.com/items?itemName=DigitalAssetHoldingsLLC.ghcide).

### Using with Emacs

If you don't already have [MELPA](https://melpa.org/#/) package installation configured, visit MELPA [getting started](https://melpa.org/#/getting-started) page to get set up. Then, install [`use-package`](https://melpa.org/#/use-package).

Now you have a choice of two different Emacs packages which can be used to communicate with the `ghcide` LSP server:

+ `lsp-ui`
+ `eglot` (requires Emacs 26.1+)

In each case, you can enable support by adding the shown lines to your `.emacs`:

#### lsp-ui

```elisp
;; LSP
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "ghcide")
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
)
```

#### eglot

````elisp
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(haskell-mode . ("ghcide" "--lsp"))))
````

### Using with Vim/Neovim

#### LanguageClient-neovim
Install [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)

Add this to your vim config:
```vim
let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rls'],
    \ 'haskell': ['ghcide', '--lsp'],
    \ }
```

Refer to `:he LanguageClient` for more details on usage and configuration.

#### vim-lsp
Install [vim-lsp](https://github.com/prabirshrestha/vim-lsp).

Add this to your vim config:

```vim
au User lsp_setup call lsp#register_server({
    \ 'name': 'ghcide',
    \ 'cmd': {server_info->['/your/path/to/ghcide', '--lsp']},
    \ 'whitelist': ['haskell'],
    \ })
```

To verify it works move your cursor over a symbol and run `:LspHover`.

### coc.nvim

Install [coc.nvim](https://github.com/neoclide/coc.nvim)

Add this to your coc-settings.json (which you can edit with :CocConfig):

```json
{
  "languageserver": {
    "haskell": {
      "command": "ghcide",
      "args": [
        "--lsp"
      ],
      "rootPatterns": [
        ".stack.yaml",
        ".hie-bios",
        "BUILD.bazel",
        "cabal.config",
        "package.yaml"
      ],
      "filetypes": [
        "hs",
        "lhs",
        "haskell"
      ]
    }
  }
}
```

Here's a nice article on setting up neovim and coc: [Vim and Haskell in
2019](http://marco-lopes.com/articles/Vim-and-Haskell-in-2019/)

### SpaceVim

In the `autocomplete` layer, add the `autocomplete_method` option to force the use of `coc`:

```toml
[[layers]]
  name = 'autocomplete'
  auto-completion-return-key-behavior = "complete"
  auto-completion-tab-key-behavior = "smart"
  [options]
    autocomplete_method = "coc"
```

Add this to your coc-settings.json (which you can edit with :CocConfig):

```json
{
  "languageserver": {
    "haskell": {
      "command": "stack",
      "args": [
        "exec",
        "ghcide",
        "--lsp"
      ],
      "rootPatterns": [
        ".stack.yaml",
        ".hie-bios",
        "BUILD.bazel",
        "cabal.config",
        "package.yaml"
      ],
      "filetypes": [
        "hs",
        "lhs",
        "haskell"
      ]
    }
  }
}
```

This example above describes a setup in which `ghcide` is installed
using `stack install ghcide` within a project.

## Hacking on ghcide

To build and work on `ghcide` itself, you can use Stack or cabal, e.g.,
running `stack test` will execute the test suite.

### Building the extension

For development, you can also the VSCode extension from this repository (see
https://code.visualstudio.com/docs/setup/mac for details on adding
`code` to your `$PATH`):

1. `cd extension/`
2. `npm ci`
3. `npm run vscepackage`
4. `code --install-extension ghcide-0.0.1.vsix`

Now opening a `.hs` file should work with `ghcide`.

## History and relationship to other Haskell IDE's

The code behind `ghcide` was originally developed by [Digital Asset](https://digitalasset.com/) as part of the [DAML programming language](https://github.com/digital-asset/daml). DAML is a smart contract language targeting distributed-ledger runtimes, based on [GHC](https://www.haskell.org/ghc/) with custom language extensions. The DAML programming language has [an IDE](https://webide.daml.com/), and work was done to separate off a reusable Haskell-only IDE (what is now `ghcide`) which the [DAML IDE then builds upon](https://github.com/digital-asset/daml/tree/master/compiler/damlc). Since that time, there have been various [non-Digital Asset contributors](https://github.com/digital-asset/ghcide/graphs/contributors), in addition to continued investment by Digital Asset. All contributions require a [Contributor License Agreement](https://cla.digitalasset.com/digital-asset/ghcide) that states you license the code under the [Apache License](LICENSE).

The Haskell community [has](https://github.com/DanielG/ghc-mod) [various](https://github.com/chrisdone/intero) [IDE](https://github.com/rikvdkleij/intellij-haskell) [choices](http://leksah.org/), but the one that has been gathering momentum is [`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine#readme). Our project owes a debt of gratitude to the `haskell-ide-engine`. We reuse libraries from their ecosystem, including [`hie-bios`](https://github.com/mpickering/hie-bios#readme) (a likely future environment setup layer in `haskell-ide-engine`), [`haskell-lsp`](https://github.com/alanz/haskell-lsp#readme) and [`lsp-test`](https://github.com/bubba/lsp-test#readme) (the `haskell-ide-engine` [LSP protocol](https://microsoft.github.io/language-server-protocol/) pieces). We make heavy use of their contributions to GHC itself, in particular the work to make GHC take string buffers rather than files. While `ghcide` is not a part of `haskell-ide-engine`, we feel it _could_ form the core of a future version - but such decisions are up to the `haskell-ide-engine` contributors.

The best summary of the architecture of `ghcide` is available [this talk](https://www.youtube.com/watch?v=cijsaeWNf2E&list=PLxxF72uPfQVRdAsvj7THoys-nVj-oc4Ss) ([slides](https://ndmitchell.com/downloads/slides-making_a_haskell_ide-07_sep_2019.pdf)), given at [MuniHac 2019](https://munihac.de/2019.html). However, since that talk the project has renamed from `hie-core` to `ghcide`, and the repo has moved to [this location](https://github.com/digital-asset/ghcide/).
