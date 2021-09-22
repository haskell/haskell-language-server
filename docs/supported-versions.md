# Supported GHC versions

## Current GHC version support status

The current support for different GHC versions is given in the following table.
 
| GHC version | Last supporting HLS version                                                                                                                              | Deprecation status                       |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------------- |
| 9.2.0       | not supported yet                                                                                                                                        |                                          |
| 9.0.1       | [current](https://github.com/haskell/haskell-language-server/releases/latest) ([partial](https://github.com/haskell/haskell-language-server/issues/297)) |                                          |
| 8.10.7      | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            |                                          |
| 8.10.6      | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | will be deprecated after LTS for ghc-9.0 |
| 8.10.5      | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | deprecated, will be removed after 1.5.0  |
| 8.10.4      | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |
| 8.10.3      | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |
| 8.10.2      | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |
| 8.10.1      | [0.9.0](https://github.com/haskell/haskell-language-server/releases/tag/0.9.0)                                                                           | deprecated                               |
| 8.8.4       | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | will be deprecated after LTS for ghc-9.2 |
| 8.8.3       | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | deprecated, will be removed after 1.5.0  |
| 8.8.2       | [1.2.0](https://github.com/haskell/haskell-language-server/releases/tag/1.2.0)                                                                           | deprecated                               |
| 8.6.5       | [current](https://github.com/haskell/haskell-language-server/releases/latest)                                                                            | will be deprecated after LTS for ghc-9.2 |
| 8.6.4       | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)                                                                           | deprecated                               |

GHC versions not in the list have never been supported by HLS, or are not planned.

The policy for when we deprecate support for versions of GHC is given below. The table reflects that, but we may decide to deviate from it for good reasons.

### Using deprecated GHC versions

Users who want to use a GHC version which is not supported by the latest HLS can still use older versions of HLS (consult the version support table above to identify the appropriate HLS version).
In the future, we may extend the existing discovery mechanisms (`haskell-language-server-wrapper`, automatic download in vscode extension) to find and download older HLS binaries in this case.

Users of a deprecated minor version (where the major version is still supported) can try building the latest HLS from source, which will likely still work, since the GHC API tends to remain compatible across minor versions.

## GHC version deprecation policy

### Major versions

A major GHC version is a "legacy" version if it is 3 or more major versions behind the GHC version used in the newest Stackage LTS. HLS will support all non-legacy major versions of GHC.

### Minor versions

For the latest supported major GHC version we will support at least 2 minor versions.

For the rest of the supported major GHC versions, we will support at least the latest minor version in Stackage LTS (so 1 minor version).

### Announcements

We will warn users about the upcoming deprecation of a GHC version in the notes of the release *prior* to the deprecation itself.

### Why deprecate older versions of GHC? 

`haskell-language-server` is highly tied to the GHC API, so much so that it needs to be specially built for each GHC minor version to ensure it will work reliably.
This means that the codebase is riddled with CPP to handle each supported ghc versions. It even needs entire compatibility packages to fully support older versions of GHC.
Moreover, our continuous integration setup has to cover all of those cases, which uses a lot of resources.

So we need to limit the number of versions of GHC that we support in order to save maintainers and contributors time and to reduce the consumption of CI resources. 
This is vital to make HLS development manageable.

At the same time we aim to support enough GHC versions to minimize the impact on end users.
This includes making an effort to support users who need to keep using old GHC versions, even if they are out of the support window offered by GHC itself.

### What factors do we take into account when deprecating a version?

To guide the policy (and possible exceptions) we aim to take in account:
- Completeness of support: all plugins and features should work with that GHC version
- GHC versions supported by newer [Stackage](https://www.stackage.org/) LTS's
- GHC versions supported by default in the most popular [Linux distributions](https://repology.org/project/ghc/versions)
- The specific history of GHC releases and their reliability on the major operating systems (Linux, Windows, MacOS)
