
# Benchmarks

This folder contains two Haskell programs that work together to simplify the
performance analysis of ghcide:

- `exe/Main.hs` - a standalone benchmark runner. Run with `stack run ghcide-bench`
- `hist/Main.hs` - a Shake script for running the benchmark suite over a set of commits.
  - Run with `stack bench ghcide` or `cabal bench ghcide`,
  - Requires a `ghcide-bench` binary in the PATH (usually provided by stack/cabal),
  - Calls `cabal` (or `stack`, configurable) internally to build the project,
  - Driven by the `bench/config.yaml` configuration file.
    By default it compares HEAD with "master"

# Examples and experiments

The benchmark suites runs a set of experiments (hover, completion, edit, etc.)
over all the defined examples (currently Cabal and lsp-types). Examples are defined
in `ghcide/bench/config.yaml` whereas experiments are coded in `ghcide/bench/lib/Experiments.hs`.

# Phony targets

The Shake script supports a number of phony targets that allow running a subset of the benchmarks:

* all
:  runs all the examples, unprofiled

* profiled-all
:  runs all the examples with heap profiling, assuming `profilingInterval` is defined

* Cabal-3.0.0.0
:  runs the Cabal example, unprofiled

* profiled-Cabal-3.0.0.0
:  runs the Cabal example, with heap profiling

* etc

`--help` lists all the phony targets. Invoke it with:

    cabal bench ghcide --benchmark-options="--help"

```
Targets:
  - bench-results/binaries/*/commitid
  - bench-results/binaries/HEAD/ghcide
  - bench-results/binaries/HEAD/ghc.path
  - bench-results/binaries/*/ghcide
  - bench-results/binaries/*/ghc.path
  - bench-results/binaries/*/*.warmup
  - bench-results/*/*/*/*.csv
  - bench-results/*/*/*/*.gcStats.log
  - bench-results/*/*/*/*.output.log
  - bench-results/*/*/*/*.eventlog
  - bench-results/*/*/*/*.hp
  - bench-results/*/*/*/results.csv
  - bench-results/*/*/results.csv
  - bench-results/*/results.csv
  - bench-results/*/*/*/*.svg
  - bench-results/*/*/*/*.diff.svg
  - bench-results/*/*/*.svg
  - bench-results/*/*/*/*.heap.svg
  - Cabal-3.0.0.0
  - lsp-types-1.0.0.1
  - all
  - profiled-Cabal-3.0.0.0
  - profiled-lsp-types-1.0.0.1
  - profiled-all
  ```
