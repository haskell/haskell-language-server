# Configuration

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

Settings like this are typically provided by the generic LSP client support for your editor, for example in Emacs by [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode).

### Generic editor options

Your editor may provide some settings that affect how the information from the language server is used.
For example, whether popups are shown, or whether code lenses appear by default.

Settings like this are typically provided by the generic LSP client support for your editor, for example in Emacs by [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode).

### Language-specific server options

A specific language server can also have its own configuration options.
These are still sent to the server by the client, but they can only be controlled by a specific client that knows about those options.

For example, `haskell-language-server` allows you to choose the formatting provider which will be used for formatting Haskell source.
This option obviously would not make sense for language servers for other languages, or even for other Haskell language servers (which need not even support formatting).

Here is a list of the additional settings currently supported by `haskell-language-server`, along with their setting key (you may not need to know this) and default:

- Formatting provider (`haskell.formattingProvider`, default `ormolu`): what formatter to use; one of `floskell`, `ormolu`, `fourmolu`, `stylish-haskell`, or `brittany` (if compiled with the brittany plugin).
- Format on imports (`haskell.formatOnImportOn`, default true): whether to format after adding an import.
- Diagnostics on change (`haskell.diagnosticsOnChange`, default true): (currently unused).
- Completion snippets (`haskell.completionSnippetsOn`, default true): whether to support completion snippets. *Deprecated* as it is equivalent to `haskell.plugin.ghcide-completions.config.snippetsOn`.
- Liquid Haskell (`haskell.liquidOn`, default false): whether to enable Liquid Haskell support (currently unused until the Liquid Haskell support is functional again, see <https://github.com/haskell/haskell-language-server/issues/367>).
- Hlint (`haskell.hlintOn`, default true): whether to enable Hlint support. *Deprecated* as it is equivalen to `haskell.plugin.hlint.globalOn`
- Max completions (`haskell.maxCompletions`, default 40): maximum number of completions sent to the LSP client.
- Check project (`haskell.checkProject`, default true): whether to typecheck the entire project on load. As it is activated by default could drive to bad perfomance in large projects.
- Check parents (`haskell.checkParents`, default `CheckOnSaveAndClose`): when to typecheck reverse dependencies of a file; one of `NeverCheck`, `CheckOnClose`, `CheckOnSaveAndClose`, or `AlwaysCheck`. 

#### Generic plugin configuration

Plugins have a generic config to control their behaviour. The schema of such config is:

- `haskell.plugin.${pluginName}.globalOn`: usually with default true. Whether the plugin is enabled at runtime or it is not. That is the option you might use if you want to disable completely a plugin.
  - Actual plugin names are: `ghcide-code-actions-fill-holes`, `ghcide-completions`, `ghcide-hover-and-symbols`, `ghcide-type-lenses`, `ghcide-code-actions-type-signatures`, `ghcide-code-actions-bindings`, `ghcide-code-actions-imports-exports`, `eval`, `moduleName`, `pragmas`, `refineImports`, `importLens`, `class`, `tactics` (aka wingman), `hlint`, `haddockComments`, `retrie`, `splice`.
  - So to disable the import lens with an explicit list of module definitions you could set `haskell.plugin.importLens.globalOn: false`
- `haskell.plugin.${pluginName}.${lspCapability}On`: usually with default true. Whether a concrete plugin capability is enabled. 
  - Capabilities are the different ways a lsp server can interact with the editor. The current available capabilities of the server are: `callHierarchy`, `codeActions`, `codeLens`, `diagnostics`, `hover`, `symbols`, `completion`, `rename`.
  - Note that usually plugins don't provide all capabilities but some of them or even only one.
  - So to disable code changes suggestions from the `hlint` plugin (but no diagnostics) you could set `haskell.plugin.hlint.codeActionsOn: false`
- Plugin specific configuration:
  - `tactic` (aka wingman):
    - `haskell.plugin.tactics.config.auto_gas`, default 4: The depth of the search tree when performing "Attempt to fill hole". Bigger values will be able to derive more solutions, but will take exponentially more time.
    - `haskell.plugin.tactics.config.timeout_duration`, default 2: The timeout for Wingman actions, in seconds.
    - `haskell.plugin.tactics.config.hole_severity`, default empty: The severity to use when showing hole diagnostics. These are noisy, but some editors don't allow jumping to all severities. One of `error`, `warning`, `info`, `hint`, `none`.
    - `haskell.plugin.tactics.config.max_use_ctor_actions`, default 5: Maximum number of `Use constructor <x>` code actions that can appear.
    - `haskell.plugin.tactics.config.proofstate_styling`, default true: Should Wingman emit styling markup when showing metaprogram proof states?
  - `ghcide-completions`:
    - `haskell.plugin.ghcide-completions.config.snippetsOn`, default true: Inserts snippets when using code completions.
    - `haskell.plugin.ghcide-completions.config.autoExtendOn`, default true: Extends the import list automatically when completing a out-of-scope identifier.
  - `ghcide-type-lenses`:
    - `haskell.plugin.ghcide-type-lenses.config.mode`, default `always`: Control how type lenses are shown. One of `always`, `exported`, `diganostics`.
  - `hlint`:
    - `haskell.plugin.hlint.config.flags`, default empty: List of flags used by hlint.
This reference of configuration can be outdated at any time but we can query the `haskell-server-executable` about what configuration is effectively used:
- `haskell-language-server generate-default-config`: will print the json configuration with all default values. It can be used as template to modify it.
- `haskell-language-server vscode-extension-schema`: will print a json schema used to setup the haskell vscode extension. But it is useful to see what range of values can an option take and a description about it.
  
Settings like this are typically provided by the language-specific LSP client support for your editor, for example in Emacs by `lsp-haskell`.

### Client options

A particular client might also have some options of its own, for example to control how the server executable is started.

Settings like this are typically be provided by the language-specific LSP client support for your editor, for example in Emacs by `lsp-haskell`.

## Configuring your project build

`haskell-language-server` has to compile your project in order to give you diagnostics, which means that it needs to know how to do so.
This is handled by the [`hie-bios`](https://github.com/mpickering/hie-bios) project.

**For a full explanation of how `hie-bios` determines the project build configuration, and how to configure it manually, refer to the [`hie-bios` README](https://github.com/mpickering/hie-bios/blob/master/README.md).**

At the moment, `haskell-language-server` has support to automatically detect your project build configuration to handle most use cases. 

However, if the automatic detection fails you can configure `hie-bios` using a `hie.yaml` file in the root of the workspace.
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
