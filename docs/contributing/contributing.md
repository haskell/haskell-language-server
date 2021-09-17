# Contributing guidelines

The Haskell tooling dream is near, we need your help!

- Join [our IRC channel](https://web.libera.chat/?channels=#haskell-language-server) at `#haskell-language-server` on [`libera`](https://libera.chat/).
- Follow the [Haskell IDE team twitter account](https://twitter.com/IdeHaskell) for updates and help.
- Join the [#haskell-tooling channel](https://discord.com/channels/280033776820813825/505370075402862594/808027763868827659) in the Functional Programming discord server. You can join the server via [this invitation](https://discord.gg/9spEdTNGrD).

## Building haskell-language-server

The project can be built with both `cabal build` and `stack build`.

haskell-language-server can also be used with itself. We provide preset samples of `hie.yaml` for Cabal and Stack.

Note: the `./install/` folder is not directly tied to the project so it has dedicated `./install/hie.yaml.[cbl|stack]`
templates.

### Using Cabal

```shell
$ cp hie-cabal.yaml hie.yaml
$ cp install/hie-cabal.yaml install/hie.yaml
```

### Using Stack

```shell
$ cp hie-stack.yaml hie.yaml
$ cp install/hie-stack.yaml install/hie.yaml
$ cp ghcide/hie-stack.yaml ghcide/hie.yaml
$ stack build --test --no-run-tests
$ cd install
$ stack build
```

### Using Nix

The instructions below show how to set up a Cachix binary cache and open a nix shell for local development.

```shell
$ cachix use haskell-language-server
$ nix-shell
$ cabal update
$ cabal build
```

#### Flakes support

If you are using nix 2.4 style command (enabled by `experimental-features = nix-command`),
you can use `nix develop` instead of `nix-shell` to enter the development shell. To enter the shell with specific GHC versions:

* `nix develop` or `nix develop .#haskell-language-server-dev` - default GHC version
* `nix develop .#haskell-language-server-8107-dev` - GHC 8.10.7
* `nix develop .#haskell-language-server-884-dev` - GHC 8.8.4
* `nix develop .#haskell-language-server-901-dev` - GHC 9.0.1

If you are looking for a Nix expression to create haskell-language-server binaries, see https://github.com/haskell/haskell-language-server/issues/122

To create binaries:

* `nix build` or `nix build .#haskell-language-server` - default GHC version
* `nix build .#haskell-language-server-8107` - GHC 8.10.7
* `nix build .#haskell-language-server-884` - GHC 8.8.4
* `nix build .#haskell-language-server-901` - GHC 9.0.1

GHC 8.6.5 is not supported here because `nixpkgs-unstable` no longer maintains the corresponding packages set.

## Testing

The tests make use of the [Tasty](https://github.com/feuerbach/tasty) test framework.

There are two test suites in the main haskell-language-server package, functional tests, and wrapper tests.
Other project packages, like the core library or plugins, can have their own test suite.

### Testing with Cabal

Running all the tests

```bash
$ cabal test
```

Running just the functional tests

```bash
$ cabal test func-test
```

Running just the wrapper tests

```bash
$ cabal test wrapper-test
```

Running a subset of tests

Tasty supports providing
[Patterns](https://github.com/feuerbach/tasty#patterns) as command
line arguments, to select the specific tests to run.

```bash
$ cabal test func-test --test-option "-p hlint"
```

The above recompiles everything every time you use a different test option though.

An alternative, which only recompiles when tests (or dependencies) change:

```bash
$ cabal run haskell-language-server:func-test -- -p "hlint enables"
```

### Test your hacked HLS in your editor

If you want to test HLS while hacking on it, follow the steps below.

To do once:

- Open some codebase on which you want to test your hacked HLS in your favorite editor
- Configure this editor to use your custom HLS executable
  - With Cabal:
    - On Unix systems: `cabal exec which haskell-language-server`
    - On Windows: `cabal exec where haskell-language-server`
  - With Stack: `$(stack path --dist-dir)/build/haskell-language-server/haskell-language-server`

To do every time you changed code and want to test it:

- Build HLS
  - With Cabal: `cabal build exe:haskell-language-server`
  - With Stack: `stack build haskell-language-server:exe:haskell-language-server`
- Restart HLS
  - With VS Code: `Haskell: Restart Haskell LSP Server`

## Style guidelines

The project includes a [`.editorconfig`](https://editorconfig.org) [file](https://github.com/haskell/haskell-language-server/blob/master/.editorconfig) with the editor basic settings used by the project.
However, most editors will need some action to honour those settings automatically.
For example vscode needs to have installed a specific [extension](https://marketplace.visualstudio.com/items?itemName=EditorConfig.EditorConfig).
Please, try to follow those basic settings to keep the codebase as uniform as possible.

### Formatter pre-commit hook

We are using [pre-commit-hook.nix](https://github.com/cachix/pre-commit-hooks.nix) to configure git pre-commit hook for formatting. Although it is possible to run formatting manually, we recommend you to use it to set pre-commit hook as our CI checks pre-commit hook is applied or not.

You can configure the pre-commit-hook by running

``` bash
nix-shell
```

If you don't want to use [nix](https://nixos.org/guides/install-nix.html), you can instead use [pre-commit](https://pre-commit.com) with the following config.

```json
{
  "repos": [
    {
      "hooks": [
        {
          "entry": "stylish-haskell --inplace",
          "exclude": "(^Setup.hs$|test/testdata/.*$|test/data/.*$|test/manual/lhs/.*$|^hie-compat/.*$|^plugins/hls-tactics-plugin/.*$|^ghcide/src/Development/IDE/GHC/Compat.hs$|^plugins/hls-splice-plugin/src/Ide/Plugin/Splice.hs$|^ghcide/test/exe/Main.hs$|ghcide/src/Development/IDE/Core/Rules.hs|^hls-test-utils/src/Test/Hls/Util.hs$)"
          "files": "\\.l?hs$",
          "id": "stylish-haskell",
          "language": "system",
          "name": "stylish-haskell",
          "pass_filenames": true,
          "types": [
            "file"
          ]
        }
      ],
      "repo": "local"
    }
  ]
}
```

#### Why some components are excluded from automatic formatting?

- `test/testdata` and `test/data` are there as we want to test formatting plugins.
- `hie-compat` is there as we want to keep its code as close to GHC as possible.
- `hls-tactics-plugin` is there as the main contributor of the plugin (@isovector) does not want auto-formatting.

## Introduction tutorial

See the [tutorial](./plugin-tutorial.md) on writing a plugin in HLS.

## Adding support for a new editor

Adding support for new editors is fairly easy if the editor already has good support for generic LSP-based extensions.
In that case, there will likely be an editor-specific support system for this (like `lsp-mode` for Emacs).
This will typically provide instructions for how to support new languages.

In some cases you may need to write a small bit of additional client support, or expose a way for the user to set the server's [configuration options](#configuring-haskell-language-server) and
for them to configure how the server is started.

## Building the docs

The docs are built with [Sphinx](https://www.sphinx-doc.org/en/master/) and [ReadTheDocs](https://docs.readthedocs.io/en/stable/index.html), the documentation for both is helpful.

To build the docs you need to install some Python prerequisites. You can either `pip install -r docs/requirements.txt`, or simply enter a `nix-shell`.

Then to build and preview the docs:

```
cd docs
make html
firefox _build/html/index.html
```

Alternatively, you can build the entire thing as a Nix derivation from the flake with `nix build .#docs`.

The docs are also built and previewed on every PR, so you can check them from the PR status.

## Sponsorship

If you want to contribute financially you can do so via [open-collective](https://opencollective.com/haskell-language-server). In the past the funding has been used to sponsor [summer student projects](https://mpickering.github.io/ide/posts/2021-07-22-summer-of-hls.html).
