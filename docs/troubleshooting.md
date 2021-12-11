# Troubleshooting

## Known limitations

### Limited GHC 9 Support
Currently, GHC 9 support in HLS is in beta stage; some features are unavailable with GHC >= 9.0.1 for the time being.
This situation is expected to be resolved in the near future after all the dependent packages support GHC 9.
See issue [#297](https://github.com/haskell/haskell-language-server/issues/297) for the supported features in GHC 9 and the status of migration progress.

### Preprocessor
HLS is not yet able to find project preprocessors, which may result in `could not execute: <preprocessor>` errors. This problem is
tracked in https://github.com/haskell/haskell-language-server/issues/176 and originally comes from https://github.com/mpickering/hie-bios/issues/125

As a workaround, you need to ensure the preprocessor is available in the path (install globally with Stack or Cabal, provide in `shell.nix`, etc.).

Example with `tasty-discover`:

```haskell
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```

This returns an error in HLS if 'tasty-discover' is not in the path: `could not execute: tasty-discover`.

### Problems with multi component (tests suites, executables, benchmarks) support using stack

Due to some limitations in the interaction between HLS and stack, there are some issues in projects with a main library and executables, test suites or benchmarks:
- The stack project has to be built succesfully *before* loading it with HLS to get components other than the library work.
- Changes in the library are not automatically propagated to other components, especially in the presence of errors in the library, so you have to restart HLS to get those components correctly loaded. The usual symptom is the editor showing errors like `Could not load module ...` or `Cannot satisfy -package ...`
- See https://github.com/haskell/haskell-language-server/issues/366 for more info about.
## Common issues

### Problems with dynamic linking

As haskell-language-server prebuilt binaries are statically linked, they don't play well with projects using dynamic linking.
An usual symptom is the presence of errors containing `unknown symbol` and it is typical in arch linux, where a dynamically linked version of ghc is used.

The workaround is to use a version of haskell-language-server compiled from source with the ghc option `-dynamic` enabled. See more details [here](https://github.com/haskell/haskell-language-server/issues/1160#issuecomment-756566273).

### Support for Template Haskell

Template Haskell should work fine in Linux and Windows with the distributed binaries. In Mac Os a dynamically linked binary of HLS is required to avoid segmentation faults. The easiest way to obtain a dynamically linked HLS binary is to build it locally. With cabal install this can be done as follows:

    cabal update && cabal install haskell-language-server --enable-executable-dynamic

## Troubleshooting the server

### Diagnostic mode

The `haskell-language-server` executable can be run in diagnostic mode, where it will just try to load modules from your project, printing all of its output to stdout.
This makes it much easier to see what's going on and to diagnose build-related problems.

To do this, simply run the executable directly from your shell in the project root.
You can either run it without an argument, in which case it will load random modules, or with a path, in which case it will load modules in that file or directory.

### Examining the log

Most clients will launch `haskell-language-server` with `--logfile` to make it write a log file.
Please consult the documentation for your client to find out where this is (or how to set it).

The log will contain all the messages that are sent to the server and its responses.
This is helpful for low-level debugging: if you expect a certain action to happen, you can look in the log to see if the corresponding messages are
sent, or if there are any errors.

To get a more verbose, also pass `--debug` to the executable.

### Identify which plugin could be the cause of the issue.

Sometimes the issue is produced by one of the plugins included in HLS. To diagnose that and help to trace the final cause one possible strategy is simple disable all plugins, check if the issue is gone and then enable them selectively until the issue is reproduced again.

There is a configuration json snippet which disables all plugins [here](https://github.com/haskell/haskell-language-server/issues/2151#issuecomment-911397030).

## Troubleshooting the client

Many clients provide diagnostic information about a LSP session.
In particular, look for a way to get the status of the server, the server stderr, or a log of the messages that the client has sent to the server.
For example, `lsp-mode` provides all of these (see its [troubleshooting page](https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/) for details).
For vscode you can read how to access the lsp session log [here])https://github.com/haskell/vscode-haskell#investigating-and-reporting-problems).

The most common client-related problem is the client simply not finding the server executable or the tools needed to load Haskell code (`ghc`, `cabal`, or `stack`). So make sure that you have the right `PATH` and you have configured the client to look for the right executables.
