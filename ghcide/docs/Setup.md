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

If you are using stack, find the list of names you can use:

    $ stack ide targets
    mypackage:lib
    mypackage:exe:mypackage-exe
    mypackage:test:mypackage-test

and create a `hie.yaml` file as follows:

    {stack: {component: "mypackage:lib"}}

## ghc: readCreateProcess: does not exist

On Linux: try `stack exec ghcide`` instead of `ghcide` directly.

I was getting this in Windows: `ghcide.exe: ghc: readCreateProcess: does not exist (No such file or directory)`

And we figured a hack around for this:

VSCode user or workspace settings, add these:

    "hic.executablePath": "stack",
    "hic.arguments": "exec ghcide -- --lsp"
    
Since I use stack. Required if you don't have a `ghc` on your path.

## Could not find module ...

Try adding an explicit `hie.yaml` file and see if that helps.

## Ambiguous main module

```console
$ stack exec ghcide

...

ghcide: CradleError (ExitFailure 1) ["Failed to parse result of calling stack","","* * * * * * * *","The main module to load is ambiguous. Candidates are: ","1. Package `mypackage' component mypackage:exe:mypackage-exe with main-is file: /home/user/mypackage/app/Main.hs","2. Package `mypackage' component mypackage:exe:otherbin-exe with main-is file: /home/user/mypackage/app/otherbin.hs","You can specify which one to pick by: "," * Specifying targets to stack ghci e.g. stack ghci mypackage:exe:mypackage-exe"," * Specifying what the main is e.g. stack ghci --main-is mypackage:exe:mypackage-exe"," * Choosing from the candidate above [1..2]","* * * * * * * *","","<stdin>: hGetLine: end of file"]
```

Add a `hie.yaml` file to specify the module, e.g.

    cradle: {stack: {component: "mypackage:exe:mypackage-exe"}}

## Works in `ghcide` but not my editor

Does `ghcide` alone work on the console? Did you first enter a Nix shell? Or run `stack exec ghcide`? If so, there are two options:

1. Run your editor via the same mechanism, e.g. `stack exec code`.
2. Change the extension to use the executable as `stack` and the arguments as `exec -- ghcide --lsp`.

## Issues with Nix

If you are using packages installed by Nix, then often Nix will set `NIX_GHC_LIBDIR` to say where the libraries are installed. `ghcide` can cope with that. However, sometimes the `ghc` on your shell will actually be a shell script that sets `NIX_GHC_LIBDIR`, which `ghcide` can't find. If that happens, you need to either set `NIX_GHC_LIBDIR` (so `ghcide` can see it) or use a proper [Nix compatible wrapper](https://github.com/hercules-ci/ghcide-nix) over `ghcide`.

## ghcide: this operation requires -fexternal-interpreter

This can happen if you have a GHC compiled without GHC library support.  This seems to be [the case](https://github.com/input-output-hk/haskell.nix/issues/313) with `haskell.nix` at the moment.

## Symbol’s value as variable is void: capability

As described [here](https://github.com/emacs-lsp/lsp-mode/issues/770#issuecomment-483540119) and [here](https://github.com/emacs-lsp/lsp-mode/issues/517#issuecomment-445448700), the default installation of `lsp-mode`, `lsp-ui`, `lsp-ui-mode` and `lsp-haskell` as described in [ghcide's "Using with Emacs" section](https://github.com/haskell/ghcide/#using-with-emacs) may result in the following error message:
 
```
Symbol’s value as variable is void: capability
```
 
This can be caused by either an old version of the Emacs package `dash`, or a cached `.elc` file for an old version. A fix consists of (re)moving the old package from ~/.emacs.d/elpa/ and installing it again, e.g. via M-x `package-list-packages` RET and M-x `package-install` RET `dash` RET. If this is not enough,
 
```
find ~/.emacs.d -name '*.elc' -exec rm {} \;
```

(which causes recompilation of all bytecode-compiled scripts.)


## Docker stack builds

You're likely to see `ghcide: (ExitFailure 1,"","")`. Because ghcide can't get at the ghc installed inside Docker, your best bet is to `stack exec ghcide` and make sure `ghcide` is installed within the container. Full details at [issue 221](https://github.com/haskell/ghcide/issues/221).

## stty error on Windows + Stack

If you get an error like:

```
ghcide.exe: CradleError (ExitFailure 1) ["Failed to parse result of calling stack","'stty' is not recognized as an internal or external command,","operable program or batch file."
```

It is fixed for stack-2.3.1 so upgrading your stack installation is the recommended action. However, there is a workaround for earlier versions described here: https://github.com/haskell/haskell-ide-engine/issues/1428#issuecomment-547530794.
