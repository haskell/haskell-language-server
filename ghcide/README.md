# `hie-core` (Haskell IDE Core)

Our vision is that you should build an IDE by combining:

* [`hie-bios`](https://github.com/mpickering/hie-bios) for determining where your files are, what are their dependencies, what extensions are enabled and so on;
* `hie-core` (i.e. this library) for defining how to type check, when to type check, and producing diagnostic messages;
* A bunch of plugins that haven't yet been written, e.g. [`hie-hlint`](https://github.com/ndmitchell/hlint) and [`hie-ormolu`](https://github.com/tweag/ormolu), to choose which features you want;
* [`haskell-lsp`](https://github.com/alanz/haskell-lsp) for sending those messages to a [Language Server Protocol (LSP)](https://microsoft.github.io/language-server-protocol/) server;
* An extension for your editor. We provide a [VS Code extension](https://code.visualstudio.com/api) as `extension` in this directory, although the components work in other LSP editors too (see below for instructions using Emacs).

There are more details about our approach [in this blog post](https://4ta.uk/p/shaking-up-the-ide).

## How to use it

First install the `hie-core` binary using `stack` or `cabal`, e.g.

1. `git clone https://github.com/digital-asset/daml.git`
2. `cd daml/compiler/hie-core`
3. `cabal install` or `stack install` (and make sure `~/.local/bin` is on your `$PATH`)

It's important that `hie-core` is compiled with the same compiler you use to build your projects.

Next, check that `hie-bios` is able to load your project. This step is currently a bit difficult.

Next, set up an extension for your editor.

### Using with VS Code

Install the VS code extension

1. `cd compiler/hie-core/extension`
2. `npm ci`
3. `npm install vsce --global` (may require `sudo`)
4. `vsce package`
5. `code --install-extension hie-core-0.0.1.vsix`

Now openning a `.hs` file should work with `hie-core`.

### Emacs

The frst step is to install required Emacs packages. If you don't already have [Melpa](https://melpa.org/#/) package installation configured in your `.emacs`, put this stanza at the top.

```elisp
;;Melpa packages support
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
;; Remember : to avoid package-not-found errors, refresh the package
;; database now and then with M-x package-refresh-contents.
```
   
When this is in your `.emacs` and evaluated, `M-x package-refresh-contents` to get the package database downloaded and then `M-x package-list-packages` to display the available packages. Click on a package to install it. You'll need to install the following packages:

* `lsp-haskell`
* `lsp-ui`
* `flycheck`
* `yasnippet`

When done with this, add the following lines to your `.emacs`:

```elisp
;; LSP support for Haskell
(require 'lsp)
(require 'lsp-haskell)
(require 'yasnippet)
(add-hook 'haskell-mode-hook #'lsp)
(setq lsp-haskell-process-path-hie "hie-core")
(setq lsp-haskell-process-args-hie '())
```

Optionally, you may wish to add the following conveniences:

```elisp
;; Enable LSP logging (helpful for debugging)
(setq lsp-log-io t)

;; Keyboard mappings for goto next/previous error
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
```

### Testing

For testing, I've been using the `ghc-lib-gen` target of the [`ghc-lib` project](https://github.com/digital-asset/ghc-lib). Navigate to the root of `ghc-lib` and create an `hie.yaml` file with contents

```yaml
cradle: {cabal: {component: "exe:ghc-lib-gen"}}
```

Invoke `cabal new-configure -w ~/.stack/programs/~/.stack/programs/x86_64-osx/ghc-8.6.5/bin/ghc` (this is the `ghc` used by `stack` to build `hie-core` - consult `//compiler/hie-core/stack.yaml` to help work out what you should write here). This last step will create a file `cabal.project.local` with contents pointing `cabal` to use the desired `ghc`. You can build `ghc-lib-gen` from the `ghc-lib` directory with the command `cabal new-build` as you like. After creating `cabal.project.local`, you should be all set. Open `ghc-lib/ghc-lib-gen/src/Main.hs` in a buffer and, for example, hover should bring up type/definition info.
