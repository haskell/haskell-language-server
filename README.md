# haskell-language-server

![haskell-language-server][logo]

[![Hackage][badge-hackage]][hackage]
[![License Apache 2.0][badge-license]][license]
[![CircleCI][badge-circleci]][circleci]
![Github Testing Workflow](https://github.com/haskell/haskell-language-server/workflows/Testing/badge.svg)
![Github Nix Workflow](https://github.com/haskell/haskell-language-server/workflows/Nix/badge.svg)

[logo]: ./docs/logos/logo-256.png
[badge-license]: https://img.shields.io/badge/license-Apache2-green.svg?dummy
[license]: https://github.com/haskell/haskell-language-server/blob/master/LICENSE
[badge-circleci]: https://img.shields.io/circleci/project/github/haskell/haskell-language-server/master.svg
[circleci]: https://circleci.com/gh/haskell/haskell-language-server/
[badge-hackage]: https://img.shields.io/hackage/v/haskell-language-server.svg?logo=haskell
[hackage]: https://hackage.haskell.org/package/haskell-language-server

Integration point for [ghcide](https://github.com/haskell/ghcide) and [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine). One IDE to rule
them all. Read the [project's
background](https://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html).

- [haskell-language-server](#haskell-language-server)
  - [Features](#features)
  - [Installation](#installation)
    - [Prerequisites](#prerequisites)
    - [ghcup](#ghcup)
    - [Visual Studio Code](#visual-studio-code)
    - [Pre-built binaries](#pre-built-binaries)
    - [Arch Linux](#arch-linux)
    - [Installation from source](#installation-from-source)
      - [Common pre-requirements](#common-pre-requirements)
      - [Linux-specific pre-requirements](#linux-specific-pre-requirements)
      - [Windows-specific pre-requirements](#windows-specific-pre-requirements)
      - [Download the source code](#download-the-source-code)
      - [Building](#building)
        - [Install via cabal](#install-via-cabal)
        - [Install specific GHC Version](#install-specific-ghc-version)
    - [Installation from Hackage](#installation-from-hackage)
  - [Configuring `haskell-language-server`](#configuring-haskell-language-server)
    - [Generic server options](#generic-server-options)
    - [Generic editor options](#generic-editor-options)
    - [Language-specific server options](#language-specific-server-options)
    - [Client options](#client-options)
  - [Configuring your project build](#configuring-your-project-build)
  - [Configuring your editor](#configuring-your-editor)
    - [VS Code](#vs-code)
    - [Sublime Text](#sublime-text)
    - [Vim or Neovim](#vim-or-neovim)
      - [Coc](#coc)
      - [LanguageClient-neovim](#languageclient-neovim)
        - [vim-plug](#vim-plug)
        - [Clone the LanguageClient-neovim repo](#clone-the-languageclient-neovim-repo)
        - [Configuration and sample `~/.vimrc` sections](#configuration-and-sample-vimrc-sections)
    - [Atom](#atom)
    - [Emacs](#emacs)
      - [doom-emacs](#doom-emacs)
      - [Spacemacs](#spacemacs)
    - [Kakoune](#kakoune)
  - [Known limitations](#known-limitations)
    - [Preprocessor](#preprocessor)
  - [Troubleshooting](#troubleshooting)
    - [Common issues](#common-issues)
      - [Difficulties with Stack and `Paths_` modules](#difficulties-with-stack-and-paths_-modules)
      - [Problems with dynamic linking](#problems-with-dynamic-linking)
    - [Troubleshooting the server](#troubleshooting-the-server)
      - [Diagnostic mode](#diagnostic-mode)
      - [Examining the log](#examining-the-log)
    - [Troubleshooting the client](#troubleshooting-the-client)
  - [Contributing](#contributing)
    - [Style guidelines](#style-guidelines)
    - [Building haskell-language-server](#building-haskell-language-server)
      - [Using Cabal](#using-cabal)
      - [Using Stack](#using-stack)
      - [Using Nix](#using-nix)
      - [Introduction tutorial](#introduction-tutorial)
      - [Test your hacked HLS in your editor](#test-your-hacked-hls-in-your-editor)
    - [Adding support for a new editor](#adding-support-for-a-new-editor)

## Features

- Code evaluation codelens ([Tutorial](plugins/hls-eval-plugin/README.md)):

   ![Eval Demo](plugins/hls-eval-plugin/demo.gif)

- Type information and documentation on hover. Note that currently, in order for docs to be displayed for dependencies, they must have been built with GHC's `-haddock` flag:

  - For cabal:
      - Add to your global config file (e.g. `~/.cabal/config`):

        ```cabal
        program-default-options
          ghc-options: -haddock
        ```

      - Or, for a single project, run `cabal configure --ghc-options=-haddock`

  - For stack, add to global `$STACK_ROOT\config.yaml`, or project's `stack.yaml`:

    ```cabal
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

### Arch Linux

If you are using Arch Linux with **dynamically linked** Haskell packages from `pacman`,
you can install the latest pre-compiled version of `haskell-language-server` from [[community]](https://archlinux.org/packages/community/x86_64/haskell-language-server/):

```
sudo pacman -S haskell-language-server
```

In this case, `haskell-language-server` is compiled against the GHC distributed to Arch Linux, so you will need maintain a system wide Haskell development environment, and install GHC from `pacman` as well.
See [ArchWiki](https://wiki.archlinux.org/index.php/Haskell) for the details of Haskell infrastructure on Arch Linux.

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
```

### Installation from Hackage

Direct installation from Hackage, while possible via `cabal install haskell-language-server`, is not recommended for most people.
Said command builds the `haskell-language-server` binary and installs it in the default Cabal binaries folder,
but the binary will only work with projects that use the same GHC version that built it.

The package can be found here on Hackage: <https://hackage.haskell.org/package/haskell-language-server>

## Configuring `haskell-language-server`

Language servers like `haskell-language-server` expose most of their configuration via the client (i.e. the editor).
That means that the way in which you configure the settings will depend on the client.

Most clients (editors) already have an opinion about how settings should be configured!
For example, in VS Code you use the graphical Settings tab or `settings.json`, whereas in Emacs you use customization variables.
In the [editor configuration section](#configuring-your-editor) we give some pointers for popular editors, but you should consult the documentation for your specific editor if you have trouble.

However, we can say some high-level things about the kinds of configuration `haskell-language-server` uses, and how to use them.
This can sound a bit confusing, but ultimately the client should present you with these options in a user-friendly way that makes sense for that editor.

### Generic server options

The LSP protocol is designed to support many useful server configuration options generically.
These are sent to the server by the client, and can be controlled without reference to a specific language.

For example, there are protocol methods for highlighting matching identifiers throughout a document.
This is a capability which any server can implement, so the client can decide generically whether to ask the server to do it or not.
So your editor can provide a setting to turn this on or off globally, for any language server you might use.

Settings like this are typically provided by the generic LSP client support for your editor, for example in Emacs by `lsp-mode`.

### Generic editor options

Your editor may provide some settings that affect how the information from the language server is used.
For example, whether popups are shown, or whether code lenses appear by default.

Settings like this are typically provided by the generic LSP client support for your editor, for example in Emacs by `lsp-mode`.

### Language-specific server options

A specific language server can also have its own configuration options.
These are still sent to the server by the client, but they can only be controlled by a specific client that knows about those options.

For example, `haskell-language-server` allows you to choose the formatting provider which will be used for formatting Haskell source.
This option obviously would not make sense for language servers for other languages, or even for other Haskell language servers (which need not even support formatting).

Here is a list of the additional settings currently supported by `haskell-language-server`, along with their setting key (you may not need to know this) and default:

- Formatting provider (`haskell.formattingProvider`, default `ormolu`): what formatter to use; one of `floskell`, `ormolu`, `fourmolu`, `stylish-haskell`, or `brittany` (if compiled with the brittany plugin)
- Format on imports (`haskell.formatOnImportOn`, default true): whether to format after adding an import
- Diagnostics on change (`haskell.diagnosticsOnChange`, default true): (currently unused)
- Completion snippets (`haskell.completionSnippetsOn`, default true): whether to support completion snippets
- Liquid Haskell (`haskell.liquidOn`, default false): whether to enable Liquid Haskell support (currently unused until the Liquid Haskell support is functional again)
- Hlint (`haskell.hlintOn`, default true): whether to enable Hlint support
- Max completions (`haskell.maxCompletions`, default 40): maximum number of completions sent to the LSP client.

Settings like this are typically provided by the language-specific LSP client support for your editor, for example in Emacs by `lsp-haskell`.

### Client options

A particular client might also have some options of its own, for example to control how the server executable is started.

Settings like this are typically be provided by the language-specific LSP client support for your editor, for example in Emacs by `lsp-haskell`.

## Configuring your project build

`haskell-language-server` has to compile your project in order to give you diagnostics, which means that it needs to know how to do so.
This is handled by the [`hie-bios`](https://github.com/mpickering/hie-bios) project.

**For a full explanation of how `hie-bios` determines the project build configuration, and how to configure it manually, refer to the [`hie-bios` README](https://github.com/mpickering/hie-bios/blob/master/README.md).**

At the moment, `haskell-language-server` has some limited support to automatically detect your project build configuration.
The plan is to improve it to handle most use cases.

However, for now, the most reliable way is to manually configure `hie-bios` using a `hie.yaml` file in the root of the workspace.
A `hie.yaml` file **explicitly** describes how to setup the environment to compile the various parts of your project.
For that you need to know what *components* your project has, and the path associated with each one.
So you will need some knowledge about
[stack](https://docs.haskellstack.org/en/stable/build_command/#components) or [cabal](https://cabal.readthedocs.io/en/latest/cabal-commands.html?#cabal-v2-build) components.

You also can use [this utility](https://github.com/Avi-D-coder/implicit-hie) to automatically generate `hie.yaml` files for
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

## Configuring your editor

Most editors provide a Haskell-specific extension that provides support for launching `haskell-language-server` and talking to it, as well as [exposing configuration options](#configuring-haskell-language-server).

Editors typically assume that you have already installed `haskell-language-server` (see above) and that the installation script put the `haskell-language-server` and `haskell-language-server-wrapper` binaries in your `PATH` (usually `~/.local/bin` or `~/.cabal/bin` on Linux and macOS, `%APPDATA%\local\bin` or `%APPDATA%\cabal\bin` on Windows).
The exception is VS Code, which can automatically install the binaries if they are not installed already.

### VS Code

Install from
[the VSCode marketplace](https://marketplace.visualstudio.com/items?itemName=haskell.haskell), or manually from the repository [vscode-haskell](https://github.com/haskell/vscode-haskell).
The `haskell-language-server` and `haskell-language-server-wrapper` binaries will be automatically downloaded on an ad-hoc basis, but if you have them already installed on your PATH then it will just use them instead.

Configuration is done via the "Haskell" section of "Settings".

### Sublime Text

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

### Vim or Neovim

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

##### Configuration and sample `~/.vimrc` sections

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

Further configuration can be done by pointing the [`g:LanguageClient_settingsPath`](https://github.com/autozimu/LanguageClient-neovim/blob/0e5c9546bfddbaa2b01e5056389c25aefc8bf989/doc/LanguageClient.txt#L221)
variable to the file in which you want to keep your LSP settings.

### Atom

Install the two Atom packages [atom-ide-ui](https://atom.io/packages/atom-ide-ui) and [haskell](https://atom.io/packages/haskell),

```bash
$ apm install language-haskell atom-ide-ui haskell
```

### Emacs

Emacs support is provided by a combination of the following packages:

[lsp-mode](https://github.com/emacs-lsp/lsp-mode)
[lsp-ui](https://github.com/emacs-lsp/lsp-ui)
[lsp-haskell](https://github.com/emacs-lsp/lsp-haskell)

You can install these manually if you are using plain Emacs; instructions for some specific flavours
are included below.

Make sure to check the READMEs of each of these packages, which explain how to configure the
various parts of the Emacs integration.
In particular, `lsp-haskell` provides customization options for the `haskell-language-server`-specific parts,
such as the path to the server executable.

#### [doom-emacs](https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/haskell#module-flags)

Manual installation of packages is not required.
Enable the lsp module and the haskell lang module with lsp flag in `.doom.d/init.el`:

``` emacs-lisp
:tools
lsp
;; ...
:lang
(haskell +lsp)
```

then do `$HOME/.emacs.d/bin/doom sync`

#### [Spacemacs](https://github.com/syl20bnr/spacemacs)

Manual installation of packages is not required.
Enable the `haskell` layer and the `lsp` layer in your Spacemacs config file:

```emacs-lisp
dotspacemacs-configuration-layers
  '(
    haskell
    lsp
    ;; ...
  )
```

### [Kakoune](https://github.com/mawww/kakoune)

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

## Troubleshooting

### Common issues

#### Difficulties with Stack and `Paths_` modules

These are known to be somewhat buggy at the moment: <https://github.com/haskell/haskell-language-server/issues/478>.
This issue should be fixed in Stack versions >= 2.5.

#### Problems with dynamic linking

As haskell-language-server prebuilt binaries are statically linked, they don't play well with projects using dynamic linking.
An usual symptom is the presence of errors containing `unknown symbol` and it is typical in arch linux, where a dynamically linked version of ghc is used.

The workaround is to use a version of haskell-language-server compiled from source with `-dynamic` enabled`. See more details [here](https://github.com/haskell/haskell-language-server/issues/1160#issuecomment-756566273).

### Troubleshooting the server

#### Diagnostic mode

The `haskell-language-server` executable can be run in diagnostic mode, where it will just try to load modules from your project, printing all of its output to stdout.
This makes it much easier to see what's going on and to diagnose build-related problems.

To do this, simply run the executable directly from your shell in the project root.
You can either run it without an argument, in which case it will load random modules, or with a path, in which case it will load modules in that file or directory.

#### Examining the log

Most clients will launch `haskell-language-server` with `--logfile` to make it write a log file.
Please consult the documentation for your client to find out where this is (or how to set it).

The log will contain all the messages that are sent to the server and its responses.
This is helpful for low-level debugging: if you expect a certain action to happen, you can look in the log to see if the corresponding messages are
sent, or if there are any errors.

To get a more verbose, also pass `--debug` to the executable.

### Troubleshooting the client

Many clients provide diagnostic information about a LSP session.
In particular, look for a way to get the status of the server, the server stderr, or a log of the messages that the client has sent to the server.
For example, `lsp-mode` provides all of these (see its [troubleshooting page](https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/) for details).

The most common client-related problem is the client simply not finding the server executable, so make sure that you have the right `PATH` and you have configured
it to look for the right executable.

## Contributing

:heart: The Haskell tooling dream is near, we need your help! :heart:

- Join [our IRC channel](https://webchat.freenode.net/?channels=haskell-language-server) at `#haskell-language-server` on `freenode`.
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
$ cp hie-cabal.yaml hie.yaml
$ cp install/hie-cabal.yaml install/hie.yaml
```

#### Using Stack

```shell
$ cp hie-stack.yaml hie.yaml
$ cp install/hie-stack.yaml install/hie.yaml
$ cp ghcide/hie-stack.yaml ghcide/hie.yaml
$ stack build --test --no-run-tests
$ cd install
$ stack build
```

#### Using Nix

The instructions below show how to set up a Cachix binary cache and open a nix shell for local development.

```shell
$ cachix use haskell-language-server
$ nix-shell
$ cabal update
$ cabal build
```

If you are looking for a Nix expression to create haskell-language-server binaries, see https://github.com/haskell/haskell-language-server/issues/122

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

### Adding support for a new editor

Adding support for new editors is fairly easy if the editor already has good support for generic LSP-based extensions.
In that case, there will likely be an editor-specific support system for this (like `lsp-mode` for Emacs).
This will typically provide instructions for how to support new languages.

In some cases you may need to write a small bit of additional client support, or expose a way for the user to set the server's [configuration options](#configuring-haskell-language-server) and
for them to configure how the server is started.

