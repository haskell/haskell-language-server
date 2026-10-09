# Contributing guidelines

The Haskell tooling dream is near, we need your help!

## How to contact the Haskell Language Server (HLS) team

- Join the [haskell-language-server channel](https://matrix.to/#/#haskell-language-server:matrix.org) on [matrix](https://matrix.org/) (primary communication channel).
- Join [our IRC channel](https://web.libera.chat/?channels=#haskell-language-server) at `#haskell-language-server` on [`libera`](https://libera.chat/) (secondary communication channel - all messages in this IRC channel are automatically bridged to the Matrix channel).
- Visit [the project GitHub repo](https://github.com/haskell/haskell-language-server) to view the source code, or open issues or pull requests.

## Building

Clone the repository:
```shell
$ git clone https://github.com/haskell/haskell-language-server
```

#### Note for contributors using WSL (Windows Subsystem for Linux)

When building HLS in WSL, clone and build the repository from the Linux filesystem (e.g. `~/dev`), not from Windows-mounted paths like `/mnt/c`, as this can cause permission or build issues.

```bash
cd ~
mkdir -p dev
cd dev
git clone https://github.com/haskell/haskell-language-server
cd haskell-language-server
```

The project can then be built with both `cabal build` and `stack build`.

### Building with Cabal

```shell
# If you have not run `cabal update` in a while
$ cabal update
# Then
$ cabal build
```

If you encounter memory-related build failures, try reducing peak memory usage by running:
```
cabal build -j1
```

### Building with Stack

```shell
$ stack build
```

### Building with Nix

The instructions below show how to set up a Cachix binary cache and open a Nix shell for local development.

```shell
$ cachix use haskell-language-server
$ nix-shell
$ cabal update
$ cabal build
```

#### Flakes support

If you are using Nix 2.4 style commands (enabled by `experimental-features = nix-command`),
you can use `nix develop` instead of `nix-shell` to enter the development shell. To enter the shell with specific GHC versions:

* `nix develop` - default GHC version,
* `nix develop .#shell-ghc90` - GHC 9.0.1 (substitute GHC version as appropriate).

If you are looking for a Nix expression to create `haskell-language-server` binaries, see https://github.com/haskell/haskell-language-server/issues/122

## Testing

The tests make use of the [Tasty](https://github.com/feuerbach/tasty) test framework.

There are two test suites in the main `haskell-language-server` package, functional tests, and wrapper tests.
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

Running just the tests for a specific plugin

```bash
$ cabal test hls-<plugin-name>-plugin-tests
# E.g.
$ cabal test hls-refactor-plugin-tests
```

Running a subset of tests

Tasty supports providing
[patterns](https://github.com/feuerbach/tasty#patterns) as command
line arguments, to select the specific tests to run.

```bash
$ cabal test func-test --test-option "-p hlint"
```

The above recompiles everything every time you use a different test option though.
An alternative, which only recompiles when tests (or dependencies) change is to pass the `TASTY_PATTERN` environment variable:

```bash
$ TASTY_PATTERN='hlint' cabal test func-test
```

## Using HLS on HLS code

Refer to the [HLS project configuration guidelines](../configuration.md#configuring-your-project-build) as they also apply to the HLS project itself.

Note: HLS implicitly detects the HLS codebase as a Stack project (since there is a `stack.yaml` file).
If you want HLS to use Cabal, create this `hie.yaml` file at the root of the project:

```yaml
cradle:
  cabal:
```

## Manually testing your hacked HLS
If you want to test HLS while hacking on it (you can even test it on HLS codebase itself, see previous section), you need to:

1. (Once) Find the path to the hacked HLS you build
2. (Once) Configure your editor to use it
3. (Every time you change the HLS code) Rebuild HLS
4. (Every time you change the HLS code) Restart the LSP workspace

### Find the path to your HLS build
Note that unless you change the GHC version or the HLS version between builds, the path should remain the same, this is why you need to set it only once.

#### Using Cabal
Run:
```shell
$ cabal build exe:haskell-language-server && cabal list-bin exe:haskell-language-server
[..]
<some long path>/haskell-language-server
```

#### Using Stack
Run:
```shell
$ echo $(pwd)/$(stack path --dist-dir)/build/haskell-language-server/haskell-language-server
[..]
<some long path>/haskell-language-server
```

### Configuring your editor to use your HLS build

#### Configuring VS Code
When using VS Code you can set up each project to use a specific HLS executable:

- If it doesn't already exist in your project directory, create a directory called `.vscode`.
- In the `.vscode` directory create a file called `settings.json` with the below contents.
```json
{
    "haskell.serverExecutablePath": "/path/to/your/hacked/haskell-language-server"
}
```

#### Configuring Emacs
There are several ways to configure the HLS server path, each of which depends on your choice of language server provider (e.g., emacs-lsp or eglot). If using emacs-lsp, you need to configure the variable `lsp-haskell-server-path`:
- `M-x customize-group<RET>lsp-haskell<RET>Lsp Haskell Server Path`
- Evaluate `(setq lsp-haskell-server-path "/path/to/your/hacked/haskell-language-server")`
- Create a file `.dir-locals.el` with the following content:
```lisp
((haskell-mode . ((lsp-haskell-server-path . "/path/to/your/hacked/haskell-language-server"))))
```

If using eglot, you need to configure the variable `eglot-server-programs`, which is an alist associating major-modes to executables:
- Evaluate `(setf (alist-get 'haskell-mode eglot-server-programs) ("/path/to/your/hacked/haskell-language-server" "--lsp"))`
- Create a file `.dir-locals.el` with the following content:
```lisp
((haskell-mode . ((eglot-server-programs . (('haskell-mode . ("/path/to/your/hacked/haskell-language-server" "--lsp")))))))
```

### Rebuild HLS
- With Stack: `stack build haskell-language-server:exe:haskell-language-server`
- With Cabal: `cabal build exe:haskell-language-server`

### Restart the LSP workspace

- With VS Code: Press `Ctrl + Shift + p` and type `Haskell: Restart Haskell LSP Server`
- With Emacs: `M-x lsp-workspace-restart`

## Style guidelines

The project includes a [`.editorconfig`](https://editorconfig.org) [file](https://github.com/haskell/haskell-language-server/blob/master/.editorconfig) with the editor basic settings used by the project.
However, most editors will need some action to honour those settings automatically.
For example VS Code needs to have installed a specific [extension](https://marketplace.visualstudio.com/items?itemName=EditorConfig.EditorConfig).
Please, try to follow those basic settings to keep the codebase as uniform as possible.

### Formatter pre-commit hook

We are using [pre-commit](https://pre-commit.com/) to configure the git pre-commit hook for formatting. Although it is possible to format code manually, we recommend you to use the pre-commit hook as our CI checks if the hook was applied or not.

If you are using Nix or Gitpod, the pre-commit hook is automatically installed. Otherwise, follow the instructions on
[https://pre-commit.com/](https://pre-commit.com/) to install the `pre-commit` tool. Then run the following command:

```sh
pre-commit install
```

#### Why are some components excluded from automatic formatting?

- `test/testdata` and `test/data` are excluded because we want to test formatting plugins.

## Plugin tutorial

See the [tutorial on writing a plugin in HLS](./plugin-tutorial.md).

## Measuring, benchmarking and tracing

### Benchmarks

If you are touching performance sensitive code, take the time to run a differential benchmark between `HEAD` and `origin/master` (see [bench/README](https://github.com/haskell/haskell-language-server/blob/master/bench/README.md)).

Run the benchmarks with `cabal bench`. The runtime is about 25 minutes and the results will be stored in the `bench-results` folder. To interpret the results, see the comments in the [bench/Main.hs](https://github.com/haskell/haskell-language-server/blob/master/bench/Main.hs) module.

### Tracing

HLS records [eventlog traces](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime_control.html#rts-eventlog) via [opentelemetry](https://hackage.haskell.org/package/opentelemetry). To generate the traces, build with `-eventlog` and run with `+RTS -l`. To visualize the traces, install [Tracy](https://github.com/wolfpld/tracy) and use [eventlog-to-tracy](https://hackage.haskell.org/package/opentelemetry-extra) to open the generated eventlog.

## Adding support for a new editor

Adding support for new editors is fairly easy if the editor already has good support for generic LSP-based extensions.
In that case, there will likely be an editor-specific support system (e.g., `lsp-mode` for Emacs).
The support system will typically provide instructions for how to add support for new languages.

In some cases you may need to write a small bit of additional client support, or expose a way for the user to set the server's [configuration options](../configuration.md#configuring-haskell-language-server) and for them to configure how the server is started.

## Building the documentation

The documentation is built with [Sphinx](https://www.sphinx-doc.org/en/master/) and [ReadTheDocs](https://docs.readthedocs.io/en/stable/index.html), the documentation of both is helpful.

You need to install some Python prerequisites. You can either `pip install -r docs/requirements.txt`, or simply enter a `nix-shell`.

Then to build and preview the documentation:

```
cd docs
make html
firefox _build/html/index.html
```

Alternatively, you can build the documentation as a Nix derivation from the Flake with `nix build .#docs`.

The documentation is also built and previewed on every PR, so you can check them from the PR status.

## Working on code actions

To make HLS easier to maintain, please follow these design guidelines when adding or modifying code actions:

1. Prefer `ghc-exactprint` to manual text parsing.
2. Prefer `ghc-exactprint` to manual code generation.
3. Code generating actions should not try to format the generated code. Assume that the user is also leveraging HLS for automated code formatting.
4. Put new code actions in their own plugin unless they are very closely aligned with an existing code action.

## Sponsorship

If you want to contribute financially, you can do so via [open-collective](https://opencollective.com/haskell-language-server). In the past, the funding was used to sponsor [summer student projects](https://mpickering.github.io/ide/posts/2021-07-22-summer-of-hls.html).
