### unreleased

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
