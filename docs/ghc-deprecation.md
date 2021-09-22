# GHC support deprecation policy

- `haskell-language-server`(HLS) is highly tied to ghc api so much so that it needs a specific flavour for each ghc minor version to ensure it will work in a reliable way
- It supposes the codebase is riddled with code conditioned to each supported ghc versions.  
  - It even needs entire packages to fully support older versions of ghc.
- Our continouos integracion setup has to cover all those cases so it have to use lot of resources to test and build the executable.
- So we need to limit the ghc support to save maintainers and contributors time and reduce ci resources consumption.  
  - It becomes vital to make HLS development manageable.
- At same time we aim to support the right balance of ghc versions to minimize impact to final users who usually needs to keep using older ghc versions, even if they are out of the window support offered by ghc itself.
- To establish the policy and the possible exceptions we aim to take in account:
   - completeness of support: the GHC flavour should include all plugins and features
   - ghc versions supported by newer [stackage](https://www.stackage.org/) LTS's
   - ghc versions supported by default in the most popular [linux distributions](https://repology.org/project/ghc/versions)
   - the specific history of ghc releases and their realibility on the major operating systems (linux, windows, macos)
- It worths to note that users of deprecated ghc versions always will have the option of:
  - Continue using the last HLS with support for their ghc version. So they will miss new bug fixes and features.
  - For deprecated minor versions of a still supported major version, try to build HLS from source.  
    - We will not guarentee it will work but will do quite likely.

## Deprecation policy

- A GHC version is legacy if it is 3 or more major versions away from the newest stackage LTS ghc version
- HLS will build on all non-legacy (major) versions of GHC,
  - for the latest major GHC version we will support at least 2 minor versions
  - for the rest of major supported GHC versions we will support at least the latest GHC minor version in stackage LTS (so 1 minor version)
- We may extend the existing discovery mechanisms (hls-wrapper, automatic download in vscode extension) to find and download older HLS binaries when it detects a legacy GHC version
- We will warn users about the deprecated ghc version in the notes of the release *prior* to the deprecation itself.
  
## Deprecation prevision

| newest LTS ghc version |            supported ghc versions               |
|------------------------|-------------------------------------------------|
| 8.10.7                 | 8.10.7, 8.10.6, 8.10.5(\*), 8.8.4, 8.8.3, 8.6.5 |
| 9.0.1                  | 9.0.1, 8.10.7, 8.8.4, 8.6.5(\*)                 |
| 9.0.2                  | 9.0.2, 9.0.1, 8.10.7, 8.8.4, 8.6.5(\*)          |
| 9.2.0                  | 9.2.0, 9.0.2, 8.10.7, 8.8.4                     |

- versions with `*` are versions which should be removed accordingly to the policy but we will keep due to the specific history of realibilty of recent ghc versions
- this prevision is not definitive and may vary depending on changes in the criteria listed above (but always honouring the policy)
