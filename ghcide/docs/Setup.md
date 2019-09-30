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

## Works in `ghcide` but not my editor

Does `ghcide` alone work on the console? Did you first enter a Nix shell? Or run `stack exec ghcide`? If so, there are two options:

1. Run your editor via the same mechanism, e.g. `stack exec code`.
2. Change the extension to use the executable as `stack` and the arguments as `exec -- ghcide --lsp`.

## Issues with Nix

If you are using packages installed by Nix, then often Nix will set `NIX_GHC_LIBDIR` to say where the libraries are installed. `ghcide` can cope with that. However, sometimes the `ghc` on your shell will actually be a shell script that sets `NIX_GHC_LIBDIR`, which `ghcide` can't find. If that happens, you need to either set `NIX_GHC_LIBDIR` (so `ghcide` can see it) or use a proper [Nix compatible wrapper](https://github.com/hercules-ci/ghcide-nix) over `ghcide`.
