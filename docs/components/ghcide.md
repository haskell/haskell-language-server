# `ghcide`

[`ghcide`](https://hackage.haskell.org/package/ghcide) is a library for building Haskell IDE tooling.

Our vision is that you should build an IDE by combining:

![vscode](https://raw.githubusercontent.com/haskell/ghcide/master/img/vscode2.png)

* [`hie-bios`](https://github.com/mpickering/hie-bios) for determining where your files are, what are their dependencies, what extensions are enabled and so on;
* `ghcide` (i.e. this library) for defining how to type check, when to type check, and producing diagnostic messages;
* A bunch of plugins that implement optional features, such as formatting, eval, linter (via `hlint`), etc...
* [`haskell-lsp`](https://github.com/haskell/lsp) for sending those messages to a [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) server;
* An LSP client for your editor.

There are more details about our approach [in this blog post](https://4ta.uk/p/shaking-up-the-ide).

## Limitations to Multi-Component support

`ghcide` supports loading multiple components into the same session so that
features such as go-to definition work across components. However, there are
some limitations to this.

1. You will get much better results currently manually specifying the hie.yaml file.
Until tools like cabal and stack provide the right interface to support multi-component
projects, it is always advised to specify explicitly how your project partitions.
2. Cross-component features only work if you have loaded at least one file
from each component.

## Using it

`ghcide` is not an end-user tool, [don't use `ghcide`](https://neilmitchell.blogspot.com/2020/09/dont-use-ghcide-anymore-directly.html) directly (more about the rationale [here](https://github.com/haskell/ghcide/pull/939)).

 [`haskell-language-server`](http://github.com/haskell/haskell-language-server) is an LSP server built on top of `ghcide` with additional features and a user friendly deployment model. To get it, simply install the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) in VS Code, or download prebuilt binaries from [GHCup](https://www.haskell.org/ghcup/).


The instructions below are meant for developers interested in setting up ghcide as an LSP server for testing purposes.

### Install `ghcide`


#### With Cabal or Stack

First install the `ghcide` binary using `stack` or `cabal`, e.g.

1. `git clone https://github.com/haskell/haskell-language-server.git`
2. `cd haskell-language-server`
3. `cabal install exe:ghcide` or `stack install ghcide` (and make sure `~/.local/bin` is on your `$PATH`)

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

The `ghcide` executable mostly relies on [`hie-bios`](https://github.com/haskell/hie-bios) to do the difficult work of setting up your GHC environment. If it doesn't work, see [the `hie-bios` manual](https://github.com/haskell/hie-bios#readme) to get it working. My default fallback is to figure it out by hand and create a `direct` style [`hie.yaml`](https://github.com/ndmitchell/shake/blob/master/hie.yaml) listing the command line arguments to load the project.

If you can't get `ghcide` working outside the editor, see [this setup troubleshooting guide](https://github.com/haskell/haskell-language-server/tree/master/ghcide/docs/Setup.md). Once you have got `ghcide` working outside the editor, the next step is to pick which editor to integrate with.

### Optimal project setup

`ghcide` has been designed to handle projects with hundreds or thousands of modules. If `ghci` can handle it, then `ghcide` should be able to handle it. The only caveat is that this currently requires GHC >= 8.8, and that the first time a module is loaded in the editor will trigger generation of support files in the background if those do not already exist.

### Using with VS Code

The [Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) extension has a setting for ghcide.

### Using with Sublime Text

* Install [LSP](https://packagecontrol.io/packages/LSP)
* Press Ctrl+Shift+P or Cmd+Shift+P in Sublime Text and search for *Preferences: LSP Settings*, then paste these settings
```
{
  "clients":
  {
    "ghcide":
    {
      "enabled"   : true,
      "languageId": "haskell",
      "command"   : ["ghcide", "--lsp"],
      "scopes"    : ["source.haskell"],
      "syntaxes"  : ["Packages/Haskell/Haskell.sublime-syntax"]
    }
  }
}
```

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

#### `LanguageClient-neovim`
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

#### `vim-lsp`
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

### `coc.nvim`

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
2019](http://marco-lopes.com/articles/Vim-and-Haskell-in-2019/) (this is actually for haskell-ide, not ghcide)

Here is a Docker container that pins down the build and configuration for
Neovim and ghcide on a minimal Debian 10 base system:
[docker-ghcide-neovim](https://github.com/carlohamalainen/docker-ghcide-neovim/).

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

This example above describes a setup in which `ghcide` is installed
using `stack install ghcide` within a project.

### Using with Kakoune

Install [kak-lsp](https://github.com/ul/kak-lsp).

Change `kak-lsp.toml` to include this:

```toml
[language.haskell]
filetypes = ["haskell"]
roots = ["Setup.hs", "stack.yaml", "*.cabal", "cabal.project", "hie.yaml"]
command = "ghcide"
args = ["--lsp"]
```

## Hacking on ghcide

To build and work on `ghcide` itself, you should use cabal, e.g.,
running `cabal test` will execute the test suite. You can use `stack test` too, but
note that some tests will fail, and none of the maintainers are currently using `stack`.

If you are using Nix, there is a Cachix nix-shell cache for all the supported platforms: `cachix use haskell-ghcide`.

If you are using Windows, you should disable the `auto.crlf` setting and configure your editor to use LF line endings, directly or making it use the existing `.editor-config`.

If you are chasing down test failures, you can use the tasty-rerun feature by running tests as

    cabal test --test-options"--rerun"

This writes a log file called `.tasty-rerun-log` of the failures, and only runs those.
See the [tasty-rerun](https://hackage.haskell.org/package/tasty-rerun-1.1.17/docs/Test-Tasty-Ingredients-Rerun.html) documentation for other options.

If you are touching performance sensitive code, take the time to run a differential
benchmark between HEAD and master using the benchHist script. This assumes that
"master" points to the upstream master.

Run the benchmarks with `cabal bench`.

It should take around 15 minutes and the results will be stored in the `bench-results` folder. To interpret the results, see the comments in the `bench/hist/Main.hs` module.

More details in [bench/README](https://github.com/haskell/haskell-language-server/tree/master/ghcide/bench/README.md)


## History and relationship to other Haskell IDE's

The teams behind this project and the [`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine#readme) have agreed to join forces under the [`haskell-language-server` project](https://github.com/haskell/haskell-language-server), see the [original announcement](https://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html). The technical work is ongoing, with the likely model being that this project serves as the core, while plugins and integrations are kept in the [`haskell-language-server` project](https://github.com/haskell/haskell-language-server).

The code behind `ghcide` was originally developed by [Digital Asset](https://digitalasset.com/) as part of the [DAML programming language](https://github.com/digital-asset/daml). DAML is a smart contract language targeting distributed-ledger runtimes, based on [GHC](https://www.haskell.org/ghc/) with custom language extensions. The DAML programming language has [an IDE](https://webide.daml.com/), and work was done to separate off a reusable Haskell-only IDE (what is now `ghcide`) which the [DAML IDE then builds upon](https://github.com/digital-asset/daml/tree/master/compiler/damlc). Since that time, there have been various [non-Digital Asset contributors](https://github.com/haskell/ghcide/graphs/contributors), in addition to continued investment by Digital Asset. The project has been handed over to Haskell.org as of September 2020.

The Haskell community [has](https://github.com/DanielG/ghc-mod) [various](https://github.com/chrisdone/intero) [IDE](https://github.com/rikvdkleij/intellij-haskell) [choices](http://leksah.org/), but the one that had been gathering momentum is [`haskell-ide-engine`](https://github.com/haskell/haskell-ide-engine#readme). Our project owes a debt of gratitude to the `haskell-ide-engine`. We reuse libraries from their ecosystem, including [`hie-bios`](https://github.com/mpickering/hie-bios#readme) (a likely future environment setup layer in `haskell-ide-engine`), [`haskell-lsp`](https://github.com/alanz/haskell-lsp#readme) and [`lsp-test`](https://github.com/bubba/lsp-test#readme) (the `haskell-ide-engine` [LSP protocol](https://microsoft.github.io/language-server-protocol/) pieces). We make heavy use of their contributions to GHC itself, in particular the work to make GHC take string buffers rather than files.

The best summary of the architecture of `ghcide` is available [this talk](https://www.youtube.com/watch?v=cijsaeWNf2E&list=PLxxF72uPfQVRdAsvj7THoys-nVj-oc4Ss) ([slides](https://ndmitchell.com/downloads/slides-making_a_haskell_ide-07_sep_2019.pdf)), given at [MuniHac 2019](https://munihac.de/2019.html). However, since that talk the project has renamed from `hie-core` to `ghcide`, and the repo has moved to [this location](https://github.com/haskell/ghcide/).
