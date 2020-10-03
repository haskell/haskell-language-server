# haskell-language-server

[![License Apache 2.0][badge-license]][license]
[![CircleCI][badge-circleci]][circleci]

[badge-license]: https://img.shields.io/badge/license-Apache2-green.svg?dummy
[license]: https://github.com/haskell/haskell-language-server/blob/master/LICENSE
[badge-circleci]: https://img.shields.io/circleci/project/github/haskell/haskell-language-server/master.svg
[circleci]: https://circleci.com/gh/haskell/haskell-language-server/

Integration point for [ghcide](https://github.com/haskell/ghcide) and [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine). One IDE to rule
them all. Read the [project's
background](https://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html).

This is *very* early stage software.

- [Haskell Language Server (HLS)](#haskell-language-server)
  - [Features](#features)
  - [Installation](#installation)
    - [Prerequisites](#prerequisites)
    - [ghcup](#ghcup)
    - [Visual Studio Code](#visual-studio-code)
    - [Pre-built binaries](#pre-built-binaries)
    - [Installation from source](#installation-from-source)
      - [Common pre-requirements](#common-pre-requirements)
      - [Linux-specific pre-requirements](#linux-specific-pre-requirements)
      - [Windows-specific pre-requirements](#windows-specific-pre-requirements)
      - [Download the source code](#download-the-source-code)
      - [Building](#building)
        - [Install via cabal](#install-via-cabal)
        - [Install specific GHC Version](#install-specific-ghc-version)
  - [HLS LSP Configuration](#hls-lsp-configuration)
  - [Project Configuration](#project-configuration)
  - [Editor Integration](#editor-integration)
    - [VS Code](#using-haskell-language-server-with-vs-code)
    - [Sublime Text](#using-haskell-language-server-with-sublime-text)
    - [Vim or Neovim](#using-haskell-language-server-with-vim-or-neovim)
      - [Coc](#coc)
      - [LanguageClient-neovim](#languageclient-neovim)
        - [vim-plug](#vim-plug)
        - [Clone the LanguageClient-neovim repo](#clone-the-languageclient-neovim-repo)
        - [Sample `~/.vimrc`](#sample-vimrc)
    - [Atom](#using-haskell-language-server-with-atom)
    - [Emacs](#using-haskell-language-server-with-emacs)
    - [Doom emacs](#using-haskell-language-server-with-doom-emacs)
    - [Kakoune](#using-haskell-language-server-with-kakoune)
  - [Known limitations](#known-limitations)
    - [Preprocessor](#preprocessor)
  - [Contributing](#contributing)
    - [Building haskell-language-server](#building-haskell-language-server)
      - [Using Cabal](#using-cabal)
      - [Using Stack](#using-stack)
      - [Introduction tutorial](#instructions-tutorial)
      - [Test your hacked HLS in your editor](#test-your-hacked-hls-in-your-editor)

## Features

 - Code evaluation codelens (inspired by [Dante](https://github.com/jyp/dante#-reploid)). You can evaluate code by writing it in a comment of the form -- >>>

   ![Eval](https://i.imgur.com/bh992sT.gif)

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

## Installation

### Prerequisites

- For standalone `.hs`/`.lhs` files, [ghc](https://www.haskell.org/ghc/) must be installed and on the PATH. The easiest way to install it is with [ghcup](https://www.haskell.org/ghcup/) or [Chocolatey](https://www.haskell.org/platform/windows.html) on Windows.
- For Cabal based projects, both ghc and [cabal-install](https://www.haskell.org/cabal/) must be installed and on the PATH. It can also be installed with [ghcup](https://www.haskell.org/ghcup/) or [Chocolatey](https://www.haskell.org/platform/windows.html) on Windows.
- For Stack based projects, [stack](http://haskellstack.org) must be installed and on the PATH.

### ghcup

If you are using [`ghcup`](https://www.haskell.org/ghcup/) to manage your installations, you can install the latest version of `haskell-language-server` with
```
ghcup install hls
```

### Visual Studio Code

If you are using Visual Studio Code, the [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell) will automatically download and install `haskell-language-server` for you.

### Pre-built binaries

There are pre-built binaries available from the [releases page](https://github.com/haskell/haskell-language-server/releases) for Linux, Windows and macOS.
To install, download the `haskell-language-server-wrapper` executable for your platform as well as any `haskell-language-server` executables for the GHC versions you plan on working with, and either put them on your PATH or point your client to them.

### Installation from source

#### Common pre-requirements

- `stack` or `cabal` must be in your PATH
  - You need stack version >= 2.1.1 or cabal >= 2.4.0.0
- `git` must be in your PATH
- The directory where `stack`or `cabal` put the binaries must be in you PATH:
  - For stack you can get it with `stack path --local-bin`
  - For cabal it is by default `$HOME/.cabal/bin` in linux and `%APPDATA%\cabal\bin` in windows.

Tip: you can quickly check if some command is in your path by running the command.
If you receive some meaningful output instead of "command not found"-like message
then it means you have the command in PATH.

#### Linux-specific pre-requirements

On Linux you will need install a couple of extra libraries:
- [Unicode (ICU)](http://site.icu-project.org/)
- [NCURSES](https://www.gnu.org/software/ncurses/)
- [Zlib](https://zlib.net/)

**Debian 9/Ubuntu 18.04 or earlier**:

```bash
sudo apt install libicu-dev libtinfo-dev libgmp-dev zlib1g-dev
```

**Debian 10/Ubuntu 18.10 or later**:

```bash
sudo apt install libicu-dev libncurses-dev libgmp-dev zlib1g-dev
```

**Fedora**:

```bash
sudo dnf install libicu-devel ncurses-devel zlib-devel
```

#### Windows-specific pre-requirements

In order to avoid problems with long paths on Windows you can do either one of the following:

1. Clone the `haskell-language-server` to a short path, for example the root of your logical drive (e.g. to
   `C:\hls`). Even if you choose `C:\haskell-language-server` you could hit the problem. If this doesn't work or you want to use a longer path, try the second option.

2. If the `Local Group Policy Editor` is available on your system, go to: `Local Computer Policy -> Computer Configuration -> Administrative Templates -> System -> Filesystem` set `Enable Win32 long paths` to `Enabled`. If you don't have the policy editor you can use regedit by using the following instructions [here](https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#enable-long-paths-in-windows-10-version-1607-and-later). You also need to configure git to allow longer paths by using unicode paths. To set this for all your git repositories use `git config --system core.longpaths true` (you probably need an administrative shell for this) or for just this one repository use `git config core.longpaths true`.

In addition make sure `haskell-language-server.exe` is not running by closing your editor, otherwise in case of an upgrade the executable can not be installed.

#### Download the source code

```bash
git clone https://github.com/haskell/haskell-language-server --recurse-submodules
cd haskell-language-server
```

#### Building

Note, on first invocation of the build script with stack, a GHC is being installed for execution.
The GHC used for the `install.hs` can be adjusted in `./install/stack.yaml` by using a different resolver.

Available commands can be seen with:

```bash
stack ./install.hs help
```

Remember, this will take time to download a Stackage-LTS and an appropriate GHC for build
haskell-language-server the first time.

##### Install via cabal

The install-script can be invoked via `cabal` instead of `stack` with the command

```bash
cabal v2-run ./install.hs --project-file install/shake.project <target>
```

or using the existing alias script

```bash
./cabal-hls-install <target>
```

Running the script with cabal on windows requires a cabal version greater or equal to `3.0.0.0`.

For brevity, only the `stack`-based commands are presented in the following sections.

##### Install specific GHC Version

The script will install the executables `haskell-language-server-wrapper` and `haskell-language-server`.

It will copy the latter appending the used ghc version, needed by the wrapper to choose the suitable version
for the project at hand.

So installing the executables directly with `stack install` or `cabal v2-install` may not be enough
for it to work properly.

Install haskell-language-server for the latest available and supported GHC version (and hoogle docs):

```bash
stack ./install.hs hls
```

Install haskell-language-server for a specific GHC version (and hoogle docs):

```bash
stack ./install.hs hls-8.8.3
stack ./install.hs data
```

`hls-8.8.3` target will build the project and install `haskell-language-server-wrapper`,
`haskell-language-server`, `haskell-language-server-8.8.3` and `haskell-language-server-8.8`
executables.

The Haskell Language Server can also be built with `cabal v2-build` instead of `stack build`.
This has the advantage that you can decide how the GHC versions have been installed.
To see what GHC versions are available, the command `cabal-hls-install ghcs` can be used.
It will list all *supported* GHC versions that are on the path for build with their respective installation directory.
If you think, this list is incomplete, you can try to modify the PATH variable, such that the executables can be found.
Note, that the targets `hls` and `data` depend on the found GHC versions.

An example output is:

```bash
> ./cabal-hls-install ghcs
******************************************************************
Found the following GHC paths:
ghc-8.6.5: /opt/bin/ghc-8.6.5
ghc-8.8.3: /opt/bin/ghc-8.8.3

******************************************************************
```

If your desired ghc has been found, you use it to install haskell-language-server.

```bash
./cabal-hls-install hls-8.6.5
./cabal-hls-install data
```

## HLS LSP Configuration

haskell-language-server supports some forms of configuration.

This configuration is done via the LSP settings you provide with your editor and/or LSP client.
Some examples include:
* in VSCode/VSCodium this is done via the `Settings` tab
* with `LanguageClient-neovim` you can point the [`g:LanguageClient_settingsPath`](https://github.com/autozimu/LanguageClient-neovim/blob/0e5c9546bfddbaa2b01e5056389c25aefc8bf989/doc/LanguageClient.txt#L221)
  variable to the file in which you want to keep your LSP settings

### Formatting providers
##### Raw LSP key name: `haskell.formattingProvider`

By default, haskell-language-server is compiled with support for several different formatters.

These include
* `floskell`
* `fourmolu`
* `ormolu`
* `stylish-haskell`
* `brittany` (if compiled with AGPL)

## Project Configuration

**For a full explanation of possible configurations, refer to [hie-bios/README](https://github.com/mpickering/hie-bios/blob/master/README.md).**

haskell-language-server has some limited support via hie-bios to detect automatically
your project configuration and set up the environment for GHC.
The plan is to improve it to handle most use cases.

However, for now, the more reliable way is using a `hie.yaml` file in the root
of the workspace to **explicitly** describe how to setup the environment.
For that you need to know what *components* have your project and the path
associated with each one. So you will need some knowledge about
[stack](https://docs.haskellstack.org/en/stable/build_command/#components) or [cabal](https://cabal.readthedocs.io/en/latest/cabal-commands.html?#cabal-v2-build) components.

You also can use [this utility](https://github.com/Avi-D-coder/implicit-hie
) to generate automatically `hie.yaml` files for
the most common stack and cabal configurations

For example, to state that you want to use `stack` then the configuration file
would look like:

```yaml
cradle:
  stack:
    component: "haskell-language-server:lib"
```

If you use `cabal` then you probably need to specify which component you want
to use.

```yaml
cradle:
  cabal:
    component: "lib:haskell-language-server"
```

If you have a project with multiple components, you can use a cabal-multi
cradle:

```yaml
cradle:
  cabal:
    - path: "./test/functional/"
      component: "haskell-language-server:func-test"
    - path: "./test/utils/"
      component: "haskell-language-server:hls-test-utils"
    - path: "./exe/Main.hs"
      component: "haskell-language-server:exe:haskell-language-server"
    - path: "./exe/Wrapper.hs"
      component: "haskell-language-server:exe:haskell-language-server-wrapper"
    - path: "./src"
      component: "lib:haskell-language-server"
    - path: "./ghcide/src"
      component: "ghcide:lib:ghcide"
    - path: "./ghcide/exe"
      component: "ghcide:exe:ghcide"
```

Equivalently, you can use stack:

```yaml
cradle:
  stack:
    - path: "./test/functional/"
      component: "haskell-language-server:func-test"
    - path: "./exe/Main.hs"
      component: "haskell-language-server:exe:haskell-language-server"
    - path: "./exe/Wrapper.hs"
      component: "haskell-language-server:exe:haskell-language-server-wrapper"
    - path: "./src"
      component: "haskell-language-server:lib"
    - path: "./ghcide/src"
      component: "ghcide:lib:ghcide"
    - path: "./ghcide/exe"
      component: "ghcide:exe:ghcide"
```

Or you can explicitly state the program which should be used to collect
the options by supplying the path to the program. It is interpreted
relative to the current working directory if it is not an absolute path.

```yaml
cradle:
  bios:
    program: ".hie-bios"
```

The complete configuration is a subset of

```yaml
cradle:
  cabal:
    component: "optional component name"
  stack:
    component: "optional component name"
  bios:
    program: "program to run"
    dependency-program: "optional program to run"
  direct:
    arguments: ["list","of","ghc","arguments"]
  default:
  none:

dependencies:
  - someDep
```

## Editor Integration

Note to editor integrators: there is a `haskell-language-server-wrapper` executable, which is installed alongside the `haskell-language-server` executable. When this is invoked in the project root directory, it attempts to work out the GHC version used in the project, and then launch the matching `haskell-language-server` executable.

All of the editor integrations assume that you have already installed `haskell-language-server` (see above) and that the installation script put the `haskell-language-server` and `haskell-language-server-wrapper` binaries in your `PATH` (usually `~/.local/bin` or `~/.cabal/bin` on Linux and macOS, `%APPDATA%\local\bin` or `%APPDATA%\cabal\bin` on Windows).

### Using Haskell Language Server with VS Code

Install from
[the VSCode marketplace](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), or manually from the repository [vscode-haskell](https://github.com/haskell/vscode-haskell).
The `haskell-language-server` and `haskell-language-server-wrapper` binaries will be automatically downloaded on an ad-hoc basis, but if you have them already installed on your PATH then it will just use them instead.

### Using Haskell Language Server with Sublime Text

- Install [LSP](https://packagecontrol.io/packages/LSP) using [Package Control](https://packagecontrol.io/)
- From Sublime Text, go to Preferences and search for LSP Settings
- Paste in these settings. Make sure to change the command path to your `haskell-language-server-wrapper`

```json
{
  "clients": {
    "haskell-language-server": {
      "command": ["haskell-language-server-wrapper", "--lsp"],
      "scopes": ["source.haskell"],
      "syntaxes": ["Packages/Haskell/Haskell.sublime-syntax"],
      "languageId": "haskell"
    }
  }
}
```

Now open a Haskell project with Sublime Text and enable Language Server in the project.
You should have these features available:

1. Errors are underlined in red
2. LSP: Show Diagnostics will show a list of hints and errors
3. LSP: Format Document will prettify the file

### Using Haskell Language Server with Vim or Neovim

You can use [Coc](https://github.com/neoclide/coc.nvim), [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)
or any other Vim Language server protocol client.
Coc is recommend since it is the only complete LSP implementation for Vim and Neovim and offers snippets and floating documentation out of the box.

#### Coc

Follow Coc's [installation instructions](https://github.com/neoclide/coc.nvim).
Then issue `:CocConfig` and add the following to your Coc config file.

```json
{
  "languageserver": {
    "haskell": {
      "command": "haskell-language-server-wrapper",
      "args": ["--lsp"],
      "rootPatterns": ["*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"],
      "filetypes": ["haskell", "lhaskell"]
    }
  }
}
```

#### LanguageClient-neovim

##### vim-plug

If you use [vim-plug](https://github.com/junegunn/vim-plug), then you can do this by e.g.,
including the following line in the Plug section of your `init.vim` or `~/.vimrc`:

```text
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh'
    \ }
```

and issuing a `:PlugInstall` command within Neovim or Vim.

##### Clone the LanguageClient-neovim repo

As an alternative to using [vim-plug](https://github.com/junegunn/vim-plug) shown above, clone [LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim)
into `~/.vim/pack/XXX/start/`, where `XXX` is just a name for your "plugin suite".

##### Sample `~/.vimrc`

```vim
set rtp+=~/.vim/pack/XXX/start/LanguageClient-neovim
let g:LanguageClient_serverCommands = { 'haskell': ['haskell-language-server-wrapper', '--lsp'] }
```

You'll probably want to add some mappings for common commands:

```vim
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>
```

Use <kbd>Ctrl+x</kbd><kbd>Ctrl+o</kbd> (`<C-x><C-o>`) to open up the auto-complete menu,
or for asynchronous auto-completion, follow the setup instructions on
[LanguageClient](https://github.com/autozimu/LanguageClient-neovim).

If you'd like diagnostics to be highlighted, add a highlight group for `ALEError`/`ALEWarning`/`ALEInfo`,
or customize `g:LanguageClient_diagnosticsDisplay`:

```vim
hi link ALEError Error
hi Warning term=underline cterm=underline ctermfg=Yellow gui=undercurl guisp=Gold
hi link ALEWarning Warning
hi link ALEInfo SpellCap
```

If you're finding that the server isn't starting at the correct project root,
it may also be helpful to also specify root markers:

```vim
let g:LanguageClient_rootMarkers = ['*.cabal', 'stack.yaml']
```

### Using Haskell Language Server with Atom

Install the two Atom packages [atom-ide-ui](https://atom.io/packages/atom-ide-ui) and [haskell](https://atom.io/packages/haskell),

```bash
$ apm install language-haskell atom-ide-ui haskell
```

### Using haskell-language-server with Emacs

Install HLS along with the following emacs packages:

[lsp-mode](https://github.com/emacs-lsp/lsp-mode)
[lsp-ui](https://github.com/emacs-lsp/lsp-ui)
[lsp-haskell](https://github.com/emacs-lsp/lsp-haskell)

Make sure to follow the instructions in the README of each of these packages.

The default `lsp-haskell-server-path` is set to `haskell-language-server-wrapper`. In 
case you would like your editor to use a specific version of the `hls` server, then this
variable can be updated. Information on other configurations can be found at 
[lsp-haskell](https://github.com/emacs-lsp/lsp-haskell)

### Using haskell-language-server with [doom-emacs](https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/haskell#module-flags)

Install haskell-language-server, and then enable haskell lang module with lsp flag in `.doom.d/init.el`

``` emacs-lisp
:lang
(haskell +lsp)
```

in your `.doom.d/config.el` file

``` emacs-lisp
(use-package lsp-haskell
 :ensure t
 :config
 (setq lsp-haskell-process-path-hie "haskell-language-server-wrapper")
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
)
```

then do `$HOME/.emacs.d/bin/doom refresh`

### Using haskell-language-server with [Kakoune](https://github.com/mawww/kakoune)

1. Grab a copy of [kak-lsp](https://github.com/ul/kak-lsp), and follow the setup instructions.
2. Point your `kak-lsp.toml` to `haskell-language-server-wrapper`.

```toml
[language.haskell]
filetypes = ["haskell"]
roots = ["Setup.hs", "stack.yaml", "*.cabal"]
command = "haskell-language-server-wrapper"
args = ["--lsp"]
```

## Known limitations

### Preprocessor
HLS is not yet able to find project preprocessors, which may result in `could not execute: <preprocessor>` errors. This problem is
tracked in https://github.com/haskell/haskell-language-server/issues/176 and originally comes from https://github.com/mpickering/hie-bios/issues/125

As a workaround, you need to ensure the preprocessor is available in the path (install globally with Stack or Cabal, provide in `shell.nix`, etc.).

Example with `tasty-discover`:
```haskell
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```
This returns an error in HLS if 'tasty-discover' is not in the path: `could not execute: tasty-discover`.

## Contributing

:heart: The Haskell tooling dream is near, we need your help! :heart:

- Join [our IRC channel](https://webchat.freenode.net/?channels=haskell-ide-engine) at `#haskell-ide-engine` on `freenode`.
- Fork this repo and [ghcide](https://github.com/haskell/ghcide) and hack as much as you can.

### Style guidelines

The project includes a [`.editorconfig`](https://editorconfig.org) [file](https://github.com/haskell/haskell-language-server/blob/master/.editorconfig) with the editor basic settings used by the project.
However, most editors will need some action to honour those settings automatically.
For example vscode needs to have installed a specific [extension](https://marketplace.visualstudio.com/items?itemName=EditorConfig.EditorConfig).
Please, try to follow those basic settings to keep the codebase as uniform as possible.

### Building haskell-language-server

The project can be built with both `cabal build` and `stack build`.

haskell-language-server can also be used with itself. We provide preset samples of `hie.yaml` for Cabal and Stack.

Note: the `./install/` folder is not directly tied to the project so it has dedicated `./install/hie.yaml.[cbl|stack]`
templates.

#### Using Cabal

```shell
$ cp hie.yaml.cbl hie.yaml
$ cp install/hie.yaml.cbl install/hie.yaml
```

#### Using Stack

Note: Stack project must also be built once until [this issue](https://github.com/commercialhaskell/stack/issues/5213) is fixed.

```shell
$ cp hie.yaml.stack hie.yaml
$ cp install/hie.yaml.stack install/hie.yaml
$ stack build --test --no-run-tests
$ cd install
$ stack build
```

#### Introduction tutorial
Pepeiborra [wrote an tutorial](https://github.com/pepeiborra/hls-tutorial) on writing a plugin in HLS.

#### Test your hacked HLS in your editor
If you want to test HLS while hacking on it, follow the steps below.

To do once:
- Open some codebase on which you want to test your hacked HLS in your favorite editor
- Configure this editor to use your custom HLS executable
  - With Cabal:
    - On Unix systems: `cabal exec which haskell-language-server`
    - On Windows: `cabal exec where haskell-language-server`
  - With Stack: `$(stack path --dist-dir)/build/haskell-language-server/haskell-language-server`

To do every time you changed code and want to test it:
- Build HLS
  - With Cabal: `cabal build exe:haskell-language-server`
  - With Stack: `stack build haskell-language-server:exe:haskell-language-server`
- Restart HLS
  - With VS Code: `Haskell: Restart Haskell LSP Server`
