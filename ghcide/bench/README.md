
# Benchmarks

This folder contains two Haskell programs that work together to simplify the
performance analysis of ghcide:

- `exe/Main.hs` - a standalone benchmark runner. Run with `stack run ghcide-bench`
- `hist/Main.hs` - a Shake script for running the benchmark suite over a set of commits.
  - Run with `stack bench` or `cabal bench`,
  - Requires a `ghcide-bench` binary in the PATH (usually provided by stack/cabal),
  - Calls `cabal` (or `stack`, configurable) internally to build the project,
  - Driven by the `config.yaml` configuration file.
    By default it compares HEAD with "master"

Further details available in the config file and the module header comments.
