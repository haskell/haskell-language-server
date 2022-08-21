A benchmark suite for measuring various performance-related metrics on ghcide and HLS.

## Usage

Run with `cabal ghcide bench`, point it to a `haskell-language-server` or `ghcide` binary, specify:
- the experiment to run, from the ones defined in `src/Experiments.hs`,
- the example codebase (either a local folder or a Hackage package),
- one or more module paths to run the experiment on,
- the number of samples,
- any extra command line options to pass to the binary,

```
Usage: ghcide-bench [(-v|--verbose) | (-q|--quiet)] [--shake-profiling PATH]
                    [--ot-profiling DIR] [--csv PATH] [--stack]
                    [--ghcide-options ARG] [-s|--select ARG] [--samples NAT]
                    [--ghcide PATH] [--timeout ARG]
                    [[--example-package-name ARG]
                      [--example-package-version ARG]
                      [(--example-module PATH)] |
                      --example-path ARG (--example-module PATH)] [--lsp-config]
                    [--no-clean]

Available options:
  --ot-profiling DIR       Enable OpenTelemetry and write eventlog for each
                           benchmark in DIR
  --stack                  Use stack (by default cabal is used)
  --ghcide-options ARG     additional options for ghcide
  -s,--select ARG          select which benchmarks to run
  --samples NAT            override sampling count
  --ghcide PATH            path to ghcide
  --timeout ARG            timeout for waiting for a ghcide response
  --lsp-config             Read an LSP config payload from standard input
  -h,--help                Show this help text
```

## Experiments

Experiments are LSP sessions defined using the `lsp-test` DSL that run on one or
more modules.

Currently the following experiments are defined:
- *edit*: makes an edit and waits for re-typechecking
- *hover*: asks for hover on an identifier
- *getDefinition*: asks for the definitions of an identifier
- *documentsymbols*
- *completions*: asks for completions on an identifier position
- *code actions*: makes an edit that breaks typechecking and asks for code actions
- *hole fit suggestions*: measures the performance of hole fits
- *X after edit*: combines the *edit* and X experiments
- *X after cradle edit*: combines the X experiments with an edit to the `hie.yaml` file

One can define additional experiments easily, for e.g. formatting, code lenses, renames, etc.
Experiments are defined in the `src/Experiments.hs` module.

### Positions
`ghcide-bench` will analyze the modules prior to running the experiments,
and try to identify the following designated source locations in the module:

- *stringLiteralP*: a location that can be mutated without generating a diagnostic,
- *identifierP*: a location with an identifier that is not locally defined in the module.
- *docP*: a location containing a comment
