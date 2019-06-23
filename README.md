# `hie-core` (Haskell IDE Core)

Our vision is that you should build an IDE by combining:

* [`hie-bios`](https://github.com/mpickering/hie-bios) for determining where your files are, what are their dependencies, what extensions are enabled and so on;
* `hie-core` (i.e. this library) for defining how to type check, when to type check, and producing messages;
* [`haskell-lsp`](https://github.com/alanz/haskell-lsp) for sending those messages to an LSP ([Language Server Protocol](https://microsoft.github.io/language-server-protocol/)) server;
* A [VS Code extension](https://code.visualstudio.com/api), e.g. `extension` in this directory (although the above components enable Haskell IDE features in other editors too).

There are more details about our approach [in this blog post](https://4ta.uk/p/shaking-up-the-ide).

## How to use it

### VS Code

#### Installing the binary

1. `git clone https://github.com/digital-asset/daml.git`
2. `cd daml/compiler/hie-core`
3. `stack build`

#### Installing the VSCode extension

1. `cd compiler/hie-core/extension`
2. `npm ci`
3. `vsce package`
4. `code --install-extension hie-core-0.0.1.vsix`

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
  - `lsp-haskell`
  - `lsp-ui`
  - `flycheck`
  - `yasnippet`

When done with this, add the following lines to your `.emacs` :
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

Next stop is to build `hie-core`. In the `daml` repository, navigate to `//compiler/hie-core` and invoked `stack build`. This will install the `hie-core` executable into a location along the lines of `$HOME/path/to/daml/compiler/hie-core/.stack-work/install/x86_64-osx/nightly-2019-05-20/8.6.5/bin/hie-core`. You want to get this executable in your `$PATH`. I achieved this with a command like `ln -s ~/path/to/compiler/hie-core/.stack-work/install/x86_64-osx/nightly-2019-05-20/8.6.5/bin/hie-core ~/.local/bin/hie-core` (because `~/.local/bin` is put into my `PATH` in my `~/.bashrc`).

Time to test things out. It's important to note that for this to work, your programs need to be compiled with the same compiler used to build `hie-core`. For testing, I've been using the `ghc-lib-gen` target of the [`ghc-lib` project](https://github.com/digital-asset/ghc-lib). Navigate to the root of `ghc-lib` and create an `hie.yaml` file with contents
```yaml
cradle: {cabal: {component: "exe:ghc-lib-gen"}}
```
Invoke `cabal new-configure -w ~/.stack/programs/~/.stack/programs/x86_64-osx/ghc-8.6.5/bin/ghc` (this is the `ghc` used by `stack` to build `hie-core` - consult `//compiler/hie-core/stack.yaml` to help work out what you should write here). This last step will create a file `cabal.project.local` with contents pointing `cabal` to use the desired `ghc`. You can build `ghc-lib-gen` from the `ghc-lib` directory with the command `cabal new-build` as you like. After creating `cabal.project.local`, you should be all set. Open `ghc-lib/ghc-lib-gen/src/Main.hs` in an Emacs buffer and, for example, hover should bring up type/definition info.
