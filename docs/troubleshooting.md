# Troubleshooting

## Help, I have no idea what these words mean

If you are new to the project, you may find it helpful to read the [introduction](./what-is-hls.md) page, which explains some of the terminology used on this page.

## Getting help

### Where to ask

Many of the developers are active on [IRC](https://web.libera.chat/?channels=#haskell-language-server).
However, the most direct way to get help is to open an [issue](https://github.com/haskell/haskell-language-server/issues).

If you can diagnose whether a problem is with HLS or with the client that you are using, then it is helpful to open an issue in the appropriate repository.
But this can be tricky, and if you're not sure then you can always open one in the HLS repository and we'll help you figure out what's going on.

### What to include

Please try and give us as much information as you can!
In particular, the more you can diagnose the problem, the better.

## Basic diagnosis steps

This section lists a few basic diagnostic steps that are almost always helpful.

Sometimes these checks may be enough to work out where the problem is.
If not, refer to the sections below about diagnosing problems with the server and client, respectively.
That will also require you to figure out is whether you are looking at an issue with the server or the client.
This can be tricky to work out: if in doubt, open an issue and we'll help you figure it out.

Typical examples of client issues:

- The wrong server binary is being launched
- Diagnostics are being shown in the wrong place

Typical examples of server issues:

- The server crashes on certain files
- A code action doesn't work the way it's supposed to

Unclear examples:

- Hover documentation looks wrong (the client might be rendering it wrong, or the server might be sending the wrong thing)
- Missing functionality (the client might not support it, or the server might not support it)

### Finding your `haskell-language-server` binary

Several of the diagnostic steps require you to run the actual `haskell-language-server` binary that is installed on your computer.

Where the binary is will depend on how you installed HLS.
Consult the [installation](./installation.md) page for help.

As usual, if you installed HLS with the wrapper, you will want to run `haskell-language-server-wrapper` instead.

### Getting basic information about your installation

Running `haskell-language-server --probe-tools` will produce useful information, such as the version of HLS that you are using.
Including this in issue reports is helpful.

### Checking that the server is running

If the server isn't running at all when you are editing a Haskell file in your project, then that suggests that the client is having difficulty launching it.
Often this means that the client is configured to run the wrong binary, or the correct one is not available in your `PATH`.

The easiest way to check whether the server is running is to use an OS process monitor to see if there is a `haskell-language-server` process running.

### Checking whether the client is connecting to the server

If the server is running, you should see some kind of indicator in your client.
In some clients (e.g. `coc`) you may need to run a command to query the client's beliefs about the server state.
If the client doesn't seem to be connected despite the server running, this may indicate a bug in the client or HLS.

### Checking whether the project is being built correctly by HLS

HLS needs to build the project correctly, with the correct flags, and if it does not do so then very little is likely to work.
A typical symptom of this going wrong is "incorrect" compilation errors being sent to the client.

If this is happening, then it usually indicates a problem in the server's configuration.

### Checking whether basic functionality is working

If everything otherwise seems to be fine, then it is useful to check whether basic functionality is working.
Hover documentation (at least including type signatures) is usually a good one to try.

### Identifying specific files that cause problems

If possible, identifying specific files that cause problems is helpful.
If you want to be really helpful, minimising the example can really speed up diagnosis.

## Diagnosing problems with the server

### Examining the server log

Most clients will launch `haskell-language-server` with `--logfile` to make it write a log file.
Please consult the documentation for your client to find out where this is (or how to set it).

The log will contain all the messages that are sent to the server and its responses.
This is helpful for low-level debugging: if you expect a certain action to happen, you can look in the log to see if the corresponding messages are sent, or if there are any errors.

To get a more verbose log, you can also pass the `--debug` argument to the server.

### Reproducing failures on the command-line

The `haskell-language-server` binary can be run from the command line.

If it is run with a specific file as an argument, it will try and load that file specifically.
If it is run without a specific file, it will try and load all the files in the project.

If you are having trouble loading one or many files in the editor, then testing it this way can help make the failure more obvious and reproducible.

Running HLS from the command-line directly also provides an easy way to get the logs (with or without `--debug`).

### Plugin-related issues

Sometimes the issue is related to one of HLS's plugins.
One strategy for diagnosing this is simply disable all plugins, check if the issue is gone and then enable them selectively until the issue is reproduced again.

There is a configuration JSON snippet which disables all plugins [here](https://github.com/haskell/haskell-language-server/issues/2151#issuecomment-911397030).

### Clearing HLS's build cache

HLS builds the dependencies of your project in a separate directory to avoid clashing with your normal build tools.
Sometimes clearing this out can help if you have persistent build problems.
The cache directory is at `$HOME/.cache/hie-bios`.
You may be able to identify a specific subdirectory that relates to your project, but it should always be safe to delete the whole thing, at worst it will cause HLS to redo build work next time it opens a project.

## Diagnosing problems with the client

The most important thing to do is to consult the client's documentation.
Usually this will provide some information about troubleshooting.

For example:

- `lsp-mode` has a [troubleshooting page](https://emacs-lsp.github.io/lsp-mode/page/troubleshooting/)
- The VSCode Haskell extension has a [troubleshooting section](https://github.com/haskell/vscode-haskell#investigating-and-reporting-problems)

Many clients provide diagnostic information about a LSP session.
In particular, look for a way to get the status of the server, the server stderr, or a log of the messages that the client has sent to the server.

## Common issues

### Wrong server binary being used

HLS needs to be compiled against the same version of GHC as is used in the project.
Normally, we ship binaries for multiple versions and `haskell-language-server-wrapper` selects the correct one.

If you see an error about HLS being compiled with the wrong version of GHC, then you either need to install the correct one (if you installed it yourself), or there is something going wrong with the wrapper selecting the right HLS binary to launch.

### Unsupported GHC version or missing binaries

HLS does not support every GHC version - there are a lot of them!
Please see the [supported versions page](./support/ghc-version-support.md) for more information, including what to do if you need binaries for a version that is not yet supported by a HLS release.

### Missing server or build tools

The most common client-related problem is the client simply not finding the server executable or the tools needed to load Haskell code (`ghc`, `cabal`, or `stack`). So make sure that you have the right `PATH` and you have configured the client to look for the right executables.

Usually this will be visible in the client's log.

### Compilation failures

Sometimes HLS will simply fail to do anything with a file, or give nonsensical error messages.
The most common cause of this is that HLS is using the wrong `hie-bios` cradle to decide how to build the project (e.g., trying to use `stack` instead of `cabal`).
The server log will show which cradle is being chosen.

Using an explicit `hie.yaml` to configure the cradle can resolve the problem, see the [configuration page](./configuration.md#configuring-your-project-build).

### Multi Cradle: No prefixes matched
The error message `Multi Cradle: No prefixes matched` usually means that implicit configuration failed.
In that case, you must use [explicit configuration](./configuration.md#configuring-your-project-build).

### Static binaries

Static binaries use the GHC linker for dynamically loading dependencies when typechecking Template Haskell code, and this can run into issues when loading shared objects linked against mismatching system libraries, or into GHC linker  bugs (mainly the Mach linker used in Mac OS, but also potentially the ELF linker).
Dynamically linked binaries (including`ghci`) use the system linker instead of the GHC linker and avoid both issues.

The easiest way to obtain a dynamically linked HLS binary is to build HLS locally. With `cabal` this can be done as follows:

```bash
cabal update && cabal install :pkg:haskell-language-server
```

Or with `stack`:

```bash
stack install haskell-language-server
```

You also can leverage `ghcup compile hls`:

```bash
ghcup compile hls -v 2.9.0.0 --ghc 9.6.5
```

### Preprocessors

HLS is [not yet](https://github.com/haskell/haskell-language-server/issues/176) able to find project preprocessors, which may result in `could not execute: <preprocessor>` errors.

As a workaround, you can ensure the preprocessor is available in `PATH` (install globally with Stack or Cabal, provide in `shell.nix`, etc.).

Example with `tasty-discover`:

```haskell
{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
```

This returns an error in HLS if `tasty-discover` is not in the path: `could not execute: tasty-discover`.

### Problems with multi component support using stack

Due to some limitations in the interaction between HLS and `stack`, there are [issues](https://github.com/haskell/haskell-language-server/issues/366) in projects with multiple components (i.e. a main library and executables, test suites or benchmarks):

- The project has to be built successfully *before* loading it with HLS to get components other than the library work.
- Changes in the library are not automatically propagated to other components, especially in the presence of errors in the library. So you have to restart HLS in order for those components to be loaded correctly. The usual symptom is the editor showing errors like `Could not load module ...` or `Cannot satisfy -package ...`.
