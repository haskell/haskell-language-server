# GHC version support

## Current GHC version support status

The current support for different GHC versions is given in the following table.

Last supporting HLS version:
- "next": this GHC version is supported in master, and will be in the next released version of HLS.
- "latest": this GHC version is one of the actively supported versions (see below) and is supported in the latest released version of HLS.
- specific version number: this GHC version is no longer one of the actively supported versions, and the last version of HLS which supports it is listed.

Support status (see the support policy below for more details):
- "initial support": support for this GHC version is underway, but it is not ready to be released yet
- "basic support": this GHC version is currently actively supported, and all [tier 1 plugins](./plugin-support.md) work
- "full support": this GHC version is currently actively supported, and most [tier 2 plugins](./plugin-support.md) work
- "deprecated": this GHC version was supported in the past, but is now deprecated

| GHC version  | Last supporting HLS version                                                          | Support status |
| ------------ | ------------------------------------------------------------------------------------ | -------------- |
| 9.12.2       | [latest](https://github.com/haskell/haskell-language-server/releases/latest)         | basic support  |
| 9.10.1       | [latest](https://github.com/haskell/haskell-language-server/releases/latest)         | full support   |
| 9.8.4        | [latest](https://github.com/haskell/haskell-language-server/releases/latest)         | full support   |
| 9.8.2        | [2.9.0.1](https://github.com/haskell/haskell-language-server/releases/tag/2.9.0.1)   | deprecated     |
| 9.8.1        | [2.6.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.6.0.0)   | deprecated   |
| 9.6.7        | [latest](https://github.com/haskell/haskell-language-server/releases/latest)         | full support   |
| 9.6.6        | [2.9.0.1](https://github.com/haskell/haskell-language-server/releases/tag/2.9.0.1)   | deprecated   |
| 9.6.5        | [2.9.0.1](https://github.com/haskell/haskell-language-server/releases/tag/2.9.0.1)   | deprecated     |
| 9.6.4        | [2.6.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.6.0.0)   | deprecated     |
| 9.6.3        | [2.5.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.5.0.0)   | deprecated     |
| 9.6.2        | [2.2.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.2.0.0)   | deprecated     |
| 9.6.1        | [2.0.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.0.0.0)   | deprecated     |
| 9.4.8        | [latest](https://github.com/haskell/haskell-language-server/releases/latest)         | full support   |
| 9.4.7        | [2.5.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.5.0.0)   | deprecated     |
| 9.4.6        | [2.2.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.2.0.0)   | deprecated     |
| 9.4.5        | [2.2.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.2.0.0)   | deprecated     |
| 9.4.4        | [1.10.0.0](https://github.com/haskell/haskell-language-server/releases/tag/1.10.0.0) | deprecated     |
| 9.4.3        | [1.9.1.0](https://github.com/haskell/haskell-language-server/releases/tag/1.9.1.0)   | deprecated     |
| 9.4.(1,2)    | [1.8.0.0](https://github.com/haskell/haskell-language-server/releases/tag/1.8.0.0)   | deprecated     |
| 9.2.8        | [2.9.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.9.0.0)   | deprecated     |
| 9.2.7        | [2.0.0.1](https://github.com/haskell/haskell-language-server/releases/tag/2.0.0.1)   | deprecated     |
| 9.2.(5,6)    | [1.9.1.0](https://github.com/haskell/haskell-language-server/releases/tag/1.9.1.0)   | deprecated     |
| 9.2.(3,4)    | [1.8.0.0](https://github.com/haskell/haskell-language-server/releases/tag/1.8.0.0)   | deprecated     |
| 9.2.(1,2)    | [1.7.0.0](https://github.com/haskell/haskell-language-server/releases/tag/1.7.0.0)   | deprecated     |
| 9.0.2        | [2.4.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.4.0.0)   | deprecated     |
| 9.0.1        | [1.6.1.0](https://github.com/haskell/haskell-language-server/releases/tag/1.6.1.0)   | deprecated     |
| 8.10.7       | [2.2.0.0](https://github.com/haskell/haskell-language-server/releases/tag/2.2.0.0)   | deprecated     |
| 8.10.6       | [1.6.1.0](https://github.com/haskell/haskell-language-server/releases/tag/1.6.1.0)   | deprecated     |
| 8.10.5       | [1.5.1](https://github.com/haskell/haskell-language-server/releases/tag/1.5.1)       | deprecated     |
| 8.10.(4,3,2) | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)       | deprecated     |
| 8.10.1       | [0.9.0](https://github.com/haskell/haskell-language-server/releases/tag/0.9.0)       | deprecated     |
| 8.8.4        | [1.8.0](https://github.com/haskell/haskell-language-server/releases/1.8.0)           | deprecated     |
| 8.8.3        | [1.5.1](https://github.com/haskell/haskell-language-server/releases/1.5.1)           | deprecated     |
| 8.8.2        | [1.2.0](https://github.com/haskell/haskell-language-server/releases/tag/1.2.0)       | deprecated     |
| 8.6.5        | [1.8.0.0](https://github.com/haskell/haskell-language-server/releases/tag/1.8.0.0)   | deprecated     |
| 8.6.4        | [1.4.0](https://github.com/haskell/haskell-language-server/releases/tag/1.4.0)       | deprecated     |

GHC versions not in the list have never been supported by HLS.
LTS stands for [Stackage](https://www.stackage.org/) Long Term Support.

The policy for when we deprecate support for versions of GHC is given below.
The table reflects that, but we may decide to deviate from it for good reasons.

### Using deprecated GHC versions

Users who want to use a GHC version which is not supported by the latest HLS can still use older versions of HLS (consult the version support table above to identify the appropriate HLS version).
In the future, we may extend the existing discovery mechanisms (`haskell-language-server-wrapper`, automatic download in vscode extension) to find and download older HLS binaries in this case.

Users of a deprecated minor version (where the major version is still supported) can try building the latest HLS from source, which will likely still work, since the GHC API tends to remain compatible across minor versions.

### Using GHC versions not yet supported in a HLS release

Some users may wish to use a version of GHC that is not yet supported by a released version of HLS.
In particular, this means that pre-built binaries will not be available for that GHC version.

The easiest thing to do in this case is to build HLS from source yourself.
This can be done easily with `ghcup`, see the examples for `ghcup compile` on the [installation page](../installation.md).

Generally, if a version of GHC is supported by HLS on master _or_ is a new minor version of a GHC version that is supported by HLS on master, then compiling from source is likely to work.
Major versions of GHC which are not supported by HLS on master are extremely unlikely to work.

## GHC version deprecation policy

### Base policy

This is the static part of the policy that can be checked by a machine.

#### Major versions

HLS will support major versions of GHC until they are older than _both_

1. The major version of GHC used in the current Stackage LTS; and
2. The major version of GHC recommended by GHCup

For example, if

1. Stackage LTS uses GHC 9.2; and
2. GHCUp recommends GHC 9.4

then HLS will support back to GHC 9.2.

#### Minor versions

For the latest supported major GHC version we will support at least 2 minor versions.

For the rest of the supported major GHC versions, we will support at least the latest minor version in Stackage LTS (so 1 minor version).

### Extended policy

This is the part of the policy that needs evaluation by a human and possibly followed
by a discussion.

#### Ecosystem factors

To establish and apply the policy we take the following ecosystem factors into account:

- Support status of HLS
- The most recent [stackage](https://www.stackage.org/) LTS snapshot
- The GHC version recommended by GHCup
- The GHC versions used in the most popular [linux distributions](https://repology.org/project/ghc/versions)
- The reliability of different ghc versions on the major operating systems (Linux, Windows, MacOS)
- The [Haskell Survey results](https://taylor.fausak.me/2022/11/18/haskell-survey-results/#s2q4)

### Supporting a GHC version beyond normal deprecation time

In cases where the base policy demands a deprecation, but ecosystem factors
suggest that it's still widely used (e.g. last [Haskell Survey results](https://taylor.fausak.me/2022/11/18/haskell-survey-results/#s2q4)),
the deprecation should be suspended for the next release and the situation be re-evaluated for the release after that.

When we decide to keep on an old version, we should track it as follows:

1. open a ticket on HLS issue tracker wrt discussing to deprecate said GHC version
    - explain the reason the GHC version wasn't deprecated (context)
    - explain the maintenance burden it causes (reason)
    - evaluate whether it impacts the next HLS release (impact)
2. discuss whether ecosystem factors changed
    - e.g. if Haskell Survey results show that 25% or more of users are still on the GHC version in question, then dropping should be avoided
3. if dropping is still undesired, but maintenance burden is also high, then set out a call-for-help and contact HF for additional funding to support this GHC version
4. if no help or funding was received within 2 releases (say, e.g. 3-6 months), then drop the version regardless

### Why deprecate older versions of GHC?

`haskell-language-server`(HLS) is highly tied to the GHC API. This imposes a high maintenance cost:

- The codebase is littered with conditional logic
- We own auxiliary packages to support older versions of GHC
- CI has to cover all the supported versions

So we need to limit the GHC support to save maintainers and contributors time and reduce CI resources.

At same time we aim to support the right balance of GHC versions to minimize the impact on users.
