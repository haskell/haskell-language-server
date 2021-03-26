# Contributors Guide

## Pre-commit hook
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
          "exclude": "(^Setup.hs$|test/testdata/.*$|test/data/.*$|^hie-compat/.*$|^plugins/hls-tactics-plugin/.*$)",
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

### Why they are excluded?

- `test/testdata` and `test/data` are there as we want to test formatting plugins.
- `hie-compat` is there as we want to keep its code as close to GHC as possible.
- `hls-tactics-plugin` is there as the main contributor of the plugin (@isovector) does not want auto-formatting.

## Testing

The tests make use of the [Tasty](https://github.com/feuerbach/tasty) test framework.

There are two test suites, functional tests, and wrapper tests.

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

An alternative is

```bash
$ cabal run haskell-language-server:func-test -- -p "hlint enables"
```
