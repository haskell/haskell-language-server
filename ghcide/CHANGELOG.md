### unreleased

### 0.2.0 (2020-06-02)

* Multi-component support (thanks @mpickering)
* Support for GHC 8.10 (thanks @sheaf and @chshersh)
* Fix some TH issues (thanks @mpickering)
* Automatically pick up changes to cradle dependencies (e.g. cabal
  files) (thanks @jinwoo)
* Track dependencies when using `qAddDependentFile` (thanks @mpickering)
* Add record fields to document symbols outline (thanks @bubba)
* Fix some space leaks (thanks @mpickering)
* Strip redundant path information from diagnostics (thanks @tek)
* Fix import suggestions for operators (thanks @eddiemundo)
* Significant reductions in memory usage by using interfaces and `.hie` files (thanks
  @pepeiborra)
* Minor improvements to completions
* More comprehensive suggestions for missing imports (thanks @pepeiborra)
* Group imports in document outline (thanks @fendor)
* Upgrade to haskell-lsp-0.22 (thanks @bubba)
* Upgrade to hie-bios 0.5 (thanks @fendor)

### 0.1.0 (2020-02-04)

* Code action for inserting new definitions (see #309).
* Better default GC settings (see #329 and #333).
* Various performance improvements (see #322 and #384).
* Improvements to hover information (see #317 and #338).
* Support GHC 8.8.2 (see #355).
* Include keywords in completions (see #351).
* Fix some issues with aborted requests (see #353).
* Use hie-bios 0.4.0 (see #382).
* Avoid stuck progress reporting (see #400).
* Only show progress notifications after 0.1s (see #392).
* Progress reporting is now in terms of the number of files rather
  than the number of shake rules (see #379).

### 0.0.6 (2020-01-10)

* Fix type in hover information for do-notation and list
  comprehensions (see #243).
* Fix hover and goto-definition for multi-clause definitions (see #252).
* Upgrade to `hie-bios-0.3` (see #257)
* Upgrade to `haskell-lsp-0.19` (see #254)
* Code lenses for missing signatures are displayed even if the warning
  has not been enabled. The warning itself will not be shown if it is
  not enabled. (see #232)
* Define `__GHCIDE__` when running CPP to allow for `ghcide`-specific
  workarounds. (see #264)
* Fix some filepath normalization issues. (see #266)
* Fix build with `shake-0.18.4` (see #272)
* Fix hover for type constructors and type classes. (see #267)
* Support custom preprocessors (see #282)
* Add support for code completions (see #227)
* Code action for removing redundant symbols from imports (see #290)
* Support document symbol requests (see #293)
* Show CPP errors as diagnostics (see #296)
* Code action for adding suggested imports (see #295)

### 0.0.5 (2019-12-12)

* Support for GHC plugins (see #192)
* Update to haskell-lsp 0.18 (see #203)
* Initial support for `TemplateHaskell` (see #222)
* Code lenses for missing signatures. These are only shown if
  `-Wmissing-signatures` is enabled. (see #224)
* Fix path normalisation on Windows (see #225)
* Fix flickering of the progress indicator (see #230)

### 0.0.4 (2019-10-20)

* Add a ``--version`` cli option (thanks @jacg)
* Update to use progress reporting as defined in LSP 3.15. The VSCode
  extension has also been updated and should now be making use of
  this.
* Properly declare that we should support code actions. This helps
  with some clients that rely on this information to enable code
  actions (thanks @jacg).
* Fix a race condition caused by sharing the finder cache between
  concurrent compilations.
* Avoid normalizing include dirs. This avoids issues where the same
  file ends up twice in the module graph, e.g., with different casing
  for drive letters.

### 0.0.3 (2019-09-21)
