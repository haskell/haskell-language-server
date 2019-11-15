# Setup Troubleshooting

This page serves as a dumping ground for setup problems and their resolutions. We recommend everyone first runs `ghcide` on the console to check what files in their project load, and only the moves on to using `ghcide` through an editor (e.g. VS Code).

## "mismatched interface file versions"

If you see a problem such as:

```console
File:     ./test/Spec.hs
Range:    1:0-1:0
Source:   typecheck
Severity: DsError
Message:
  test/Spec.hs:1:1: error:
  Bad interface file:
  /Users/daml/.stack/programs/x86_64-osx/ghc-8.6.4/lib/ghc-8.6.4/base-4.12.0.0/Prelude.hi
  mismatched interface file versions (wanted "8065", got "8064")
```

The cause is that your program is configured to use a different GHC to the one you built `ghcide` with. In `ghcide` you can view the version number it was compiled with on the first line as:

```console
ghcide version: 0.0.3 (GHC: 8.6.5)
```

You can see the version of GHC being used by this project in the second-last line of the output with `ghc-8.6.4/`, or in in mismatch interfaces of wanted `8065` (aka 8.6.5), got `8064` (aka 8.6.4). The solution is to use the same GHC version in both places.

## “failed to load interface for ‘…’ There are files missing”

If you see a problem such as:

```console
File:     ./src/File/FileStream.hs
Range:    1:0-100001:0
Source:   typecheck
Severity: DsError
Message: 
  Program error: Failed to load interface for ‘Data.DList’
Files that failed:
  There are files missing in the ‘dlist-0.8.0.7’ package,
 * ./src/File/FileStream.hs
  try running 'ghc-pkg check'.
  Use -v to see a list of the files searched for.
```

It might be caused by `ghcide` picking up the wrong cradle. In
particular, this has been observed when running in a `nix-shell` where
`ghcide` picked up the default cradle. Try setting the cradle
explicitly, e.g., to use the cabal cradle create a `hie.yaml` file
with the following content:

```
cradle: {cabal: {component: "mylibrary"}}
```

## Works in `ghcide` but not my editor

Does `ghcide` alone work on the console? Did you first enter a Nix shell? Or run `stack exec ghcide`? If so, there are two options:

1. Run your editor via the same mechanism, e.g. `stack exec code`.
2. Change the extension to use the executable as `stack` and the arguments as `exec -- ghcide --lsp`.

## Issues with Nix

If you are using packages installed by Nix, then often Nix will set `NIX_GHC_LIBDIR` to say where the libraries are installed. `ghcide` can cope with that. However, sometimes the `ghc` on your shell will actually be a shell script that sets `NIX_GHC_LIBDIR`, which `ghcide` can't find. If that happens, you need to either set `NIX_GHC_LIBDIR` (so `ghcide` can see it) or use a proper [Nix compatible wrapper](https://github.com/hercules-ci/ghcide-nix) over `ghcide`.

## Symbol’s value as variable is void: capability

As described [here](https://github.com/emacs-lsp/lsp-mode/issues/770#issuecomment-483540119) and [here](https://github.com/emacs-lsp/lsp-mode/issues/517#issuecomment-445448700), the default installation of `lsp-mode`, `lsp-ui`, `lsp-ui-mode` and `lsp-haskell` as described in [ghcide's "Using with Emacs" section](https://github.com/digital-asset/ghcide/#using-with-emacs) may result in the following error message:
 
```
Symbol’s value as variable is void: capability
```
 
This can be caused by either an old version of the Emacs package `dash`, or a cached `.elc` file for an old version. A fix consists of (re)moving the old package from ~/.emacs.d/elpa/ and installing it again, e.g. via M-x `package-list-packages` RET and M-x `package-install` RET `dash` RET. If this is not enough,
 
```
find ~/.emacs.d -name '*.elc' -exec rm {} \;
```

(which causes recompilation of all bytecode-compiled scripts.)
