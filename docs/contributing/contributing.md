# Contributing guidelines

The Haskell tooling dream is near, we need your help!

## How to contact the haskell ide team

- Join [our IRC channel](https://web.libera.chat/?channels=#haskell-language-server) at `#haskell-language-server` on [`libera`](https://libera.chat/).
- Follow the [Haskell IDE team twitter account](https://twitter.com/IdeHaskell) for updates and help.
- Join the [#haskell-tooling channel](https://discord.com/channels/280033776820813825/505370075402862594/808027763868827659) in the Functional Programming discord server. You can join the server via [this invitation](https://discord.gg/9spEdTNGrD).
- Join the [haskell-tooling channel](https://matrix.to/#/#haskell-tooling:matrix.org) in [matrix](https://matrix.org/).
- Visit [the project GitHub repo](https://github.com/haskell/haskell-language-server) to view the source code, or open issues or pull requests.

## Building

Clone the repository:
```shell
$ git clone https://github.com/haskell/haskell-language-server
```

The project can then be built with both `cabal build` and `stack build`.

### Using Cabal

```shell
# If you have not run `cabal update` in a while
$ cabal update
# Then
$ cabal build
```

### Using Stack

```shell
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
* `nix develop .#haskell-language-server-901-dev` - GHC 9.0.1 (substitute GHC version as appropriate)

If you are looking for a Nix expression to create haskell-language-server binaries, see https://github.com/haskell/haskell-language-server/issues/122

To create binaries:

* `nix build` or `nix build .#haskell-language-server` - default GHC version
* `nix build .#haskell-language-server-901` - GHC 9.0.1 (substitute GHC version as appropriate)

## Testing

The tests make use of the [Tasty](https://github.com/feuerbach/tasty) test framework.

There are two test suites in the main haskell-language-server package, functional tests, and wrapper tests.
Some of the wrapper tests expect `stack` to be present on the system, or else they fail.
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

## Using HLS on HLS code

Project source code should load without `hie.yaml` setup.

In other cases:

1. Check if `hie.yaml` (& `hie.yml`) files left from previous configurations.

2. If the main project needs special configuration, note that other internal subprojects probably also would need configuration.

To create an explicit configuration for all projects - use [implicit-hie](https://github.com/Avi-D-coder/implicit-hie) generator directly:

```shell
gen-hie > hie.yaml  # into the main HLS directory
```

that configuration should help.

3. Inspect & tune configuration explicitly.

[Configuring project build](../configuration.md#configuring-your-project-build) applies to HLS project source code loading just as to any other.

Note: HLS may implicitly detect codebase as a Stack project (see [hie-bios implicit configuration documentation](https://github.com/haskell/hie-bios/blob/master/README.md#implicit-configuration)). To use Cabal, try creating an `hie.yaml` file:

```yaml
cradle:
  cabal:
```

### Manually testing your hacked HLS
If you want to test HLS while hacking on it, follow the steps below.

To do once:

- Open some codebase on which you want to test your hacked HLS in your favorite editor (it can also be HLS codebase itself: see previous section for configuration)
- Configure this editor to use your custom HLS executable
  - With Cabal:
    - On Unix systems: `cabal exec which haskell-language-server`
    - On Windows: `cabal exec where haskell-language-server`
  - With Stack: `$(stack path --dist-dir)/build/haskell-language-server/haskell-language-server`

To do every time you change HLS code and want to test it:

- Build HLS
  - With Cabal: `cabal build exe:haskell-language-server`
  - With Stack: `stack build haskell-language-server:exe:haskell-language-server`
- Restart HLS
  - With VS Code: `Haskell: Restart Haskell LSP Server`
  - With Emacs: `lsp-workspace-restart`

## Style guidelines

The project includes a [`.editorconfig`](https://editorconfig.org) [file](https://github.com/haskell/haskell-language-server/blob/master/.editorconfig) with the editor basic settings used by the project.
However, most editors will need some action to honour those settings automatically.
For example vscode needs to have installed a specific [extension](https://marketplace.visualstudio.com/items?itemName=EditorConfig.EditorConfig).
Please, try to follow those basic settings to keep the codebase as uniform as possible.

### Formatter pre-commit hook

We are using [pre-commit](https://pre-commit.com/) to configure git pre-commit hook for formatting. Although it is possible to run formatting manually, we recommend you to use it to set pre-commit hook as our CI checks pre-commit hook is applied or not.

If you are using Nix or Gitpod, pre-commit hook is automatically installed. Otherwise, follow instructions on
[https://pre-commit.com/](https://pre-commit.com/) to install the `pre-commit` tool, then run the following command:

```sh
pre-commit install
```

#### Why some components are excluded from automatic formatting?

- `test/testdata` and `test/data` are there as we want to test formatting plugins.
- `hie-compat` is there as we want to keep its code as close to GHC as possible.
- `hls-tactics-plugin` is there as the main contributor of the plugin (@isovector) does not want auto-formatting.

## Introduction tutorial

See the [tutorial](./plugin-tutorial.md) on writing a plugin in HLS.

## Measuring, benchmarking and tracing

### Metrics

When ghcide is built with the `ekg` flag, HLS opens a metrics server on port 8999 exposing GC and ghcide metrics. The ghcide metrics currently exposed are:

- `ghcide.values_count` - count of build results in the store
- `ghcide.database_count` - count of build keys in the store (these two would be the same in the absence of GC)
- `ghcide.build_count` - build count. A key is GC'ed if it is dirty and older than 100 builds
- `ghcide.dirty_keys_count` - non transitive count of dirty build keys
- `ghcide.indexing_pending_count` - count of items in the indexing queue
- `ghcide.exports_map_count` - count of identifiers in the exports map.

### Benchmarks

If you are touching performance sensitive code, take the time to run a differential
benchmark between HEAD and master using the benchHist script. This assumes that
"master" points to the upstream master.

Run the benchmarks with `cabal bench`.

It should take around 25 minutes and the results will be stored in the `bench-results` folder. To interpret the results, see the comments in the `bench/Main.hs` module.

More details in [bench/README](../../bench/README.md)

### Tracing

HLS records opentelemetry [eventlog traces](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-eventlog) via [opentelemetry](https://hackage.haskell.org/package/opentelemetry). To generate the traces, build with `-eventlog` and run with `+RTS -l`. To visualize the traces, install [Tracy](https://github.com/wolfpld/tracy) and use [eventlog-to-tracy](https://hackage.haskell.org/package/opentelemetry-extra) to open the generated eventlog.

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

## Working on code actions

To make HLS easier to maintain, please follow these design guidelines when adding or modifying code actions:

1. Prefer `ghc-exactprint` to manual text parsing.
2. Prefer `ghc-exactprint` to manual code generation.
3. Code generating actions should not try to format the generated code. Assume that the user is also leveraging HLS for automated code formatting.
4. Put new code actions in their own plugin unless they are very closely aligned with an existing ghcide code action.

## Sponsorship

If you want to contribute financially you can do so via [open-collective](https://opencollective.com/haskell-language-server). In the past the funding has been used to sponsor [summer student projects](https://mpickering.github.io/ide/posts/2021-07-22-summer-of-hls.html).
