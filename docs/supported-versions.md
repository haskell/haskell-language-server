# Supported GHC versions

## Current GHC version support status

The current support for different GHC versions is given in the following table.

| GHC version | Last supporting HLS version                                                                                                                              | Deprecation status                       |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------- |
| 9.2.0       | [not supported](https://github.com/haskell/haskell-language-server/issues/2179) yet                                                                                                                                        |                                          |
| 9.0.1       | [current](https://github.com/haskell/haskell-language-server/releases/latest) ([partial](https://github.com/haskell/haskell-language-server/issues/297)) |                                          |
| 8.10.7      | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            |                                          |
| 8.10.6      | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | will be deprecated after LTS and HLS full support for ghc-9.0 |
| 8.10.5      | [1.5.1](https://github.com/haskell/haskell-language-server/releases/tag/1.5.1)                                                                           | deprecated                               |
| 8.10.4      | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |
| 8.10.3      | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |
| 8.10.2      | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |
| 8.10.1      | [0.9.0](https://github.com/haskell/haskell-language-server/releases/tag/0.9.0)                                                                           | deprecated                               |
| 8.8.4       | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | will be deprecated after LTS and HLS full support for ghc-9.2 |
| 8.8.3       | [1.5.1](https://github.com/haskell/haskell-language-server/releases/1.5.1)                                                                               | deprecated                               |
| 8.8.2       | [1.2.0](https://github.com/haskell/haskell-language-server/releases/tag/1.2.0)                                                                           | deprecated                               |
| 8.6.5       | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | will be deprecated after LTS and HLS full suppot for ghc-9.2 |
| 8.6.4       | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |

GHC versions not in the list have never been supported by HLS, or are not planned. LTS stands for [Stackage](https://www.stackage.org/) Long Term Support.

The policy for when we deprecate support for versions of GHC is given below. The table reflects that, but we may decide to deviate from it for good reasons.

### Using deprecated GHC versions

Users who want to use a GHC version which is not supported by the latest HLS can still use older versions of HLS (consult the version support table above to identify the appropriate HLS version).
In the future, we may extend the existing discovery mechanisms (`haskell-language-server-wrapper`, automatic download in vscode extension) to find and download older HLS binaries in this case.

Users of a deprecated minor version (where the major version is still supported) can try building the latest HLS from source, which will likely still work, since the GHC API tends to remain compatible across minor versions.

## GHC version deprecation policy

### Major versions

A major GHC version is a "legacy" version if it is 3 or more major versions behind the GHC version used in the newest Stackage LTS.

HLS will support all non-legacy major versions of GHC.

### Minor versions

For the latest supported major GHC version we will support at least 2 minor versions.

For the rest of the supported major GHC versions, we will support at least the latest minor version in Stackage LTS (so 1 minor version).

### Announcements

We will warn users about the upcoming deprecation of a GHC version in the notes of the release *prior* to the deprecation itself.

### Why deprecate older versions of GHC?

`haskell-language-server`(HLS) is highly tied to the ghc api. This imposes a high maintenance cost:

- The codebase is littered with conditional logic,
- We own auxiliary packages to support older versions of ghc.
- CI has to cover all the supported versions.

So we need to limit the ghc support to save maintainers and contributors time and reduce CI resources.

At same time we aim to support the right balance of ghc versions to minimize impact to final users.

### What factors do we take into account when deprecating a version?

To establish and apply the policy we take into account:

- Completeness: support includes all plugins and features
- The most recent [stackage](https://www.stackage.org/) LTS snapshot
- The GHC versions used in the most popular [linux distributions](https://repology.org/project/ghc/versions)
- The reliability of different ghc versions on the major operating systems (Linux, Windows, MacOS)
