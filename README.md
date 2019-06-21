# Haskell IDE Core

Our vision is that you should build an IDE by combining:

* [hie-bios](https://github.com/mpickering/hie-bios) for determining where your files are, what the dependencies, what extensions are enabled etc.
* `hie-core` - this library - for defining how to type check, when to type check, and producing messages.
* `haskell-lsp` for sending those messages to an LSP server.
* A VS Code extension, e.g. `extension` in this directory.

There are more details [in this blog post](https://4ta.uk/p/shaking-up-the-ide).

## How to use it

### Installing the binary

1. `git clone https://github.com/digital-asset/daml.git`
2. `cd daml/compiler/hie-core`
3. `stack build`

### Installing the VSCode extension

1. `cd compiler/hie-core/extension`
2. `vsce package`
3. `code --install-extension hie-core-0.0.1.vsix`

### Installing in Emacs

1. Install lsp and haskell-lsp
2. Add this elisp to your .emacs.el
```elisp
(require 'lsp)
(require 'lsp-haskell)
(require 'yasnippet)
(add-hook 'haskell-mode-hook #'lsp)
(setq lsp-haskell-process-path-hie "hie-core")
(setq lsp-haskell-process-args-hie '())
```
