# Plugin support

## Plugin support tiers

Plugins vary in how well-supported they are, in particular how quickly they are updated to support new GHC versions.
This is important to keep track of because we want to release new versions of HLS for new GHC versions quickly, but also to present a consistent set of features.

For this reason we group plugins into _support tiers_.

**Tier 1**

A tier 1 plugin is a plugin which we believe is so essential to the functioning of HLS that we should not release HLS unless the plugin is working for all supported GHC versions.

Tier 1 plugins must be well-supported, or else we will be blocked from releasing HLS.
If we are not able to maintain tier 1 plugins, then we have a critical maintenance problem.
Consequently, few plugins should be considered tier 1.

**Tier 2**

A tier 2 plugin is a plugin which is important or well-enough maintained that we usually will not release HLS unless the plugin is working for all supported GHC versions.

Tier 2 plugins should be well-supported enough to usually make the cut for HLS releases, but we will not hold a release for one.

Tier 2 plugins provide a decent experience for users, since they can (mostly) rely on them being present in a release.
Hence, most plugins should ideally be tier 2.

**Tier 3**

A tier 3 plugin is anything else.

Tier 3 plugins are maintained on a best-effort basis, often by irregular contributors.
A plugin may have to be tier 3 despite being well-maintained if it depends on a tool (e.g. a formatter) which is not itself reliably updated for new GHC versions.

Since we cannot make any guarantees that a tier 3 plugin will be working, they provide a bad experience for users.
Hence a tier 3 plugin should ideally have some kind of plan for getting out of tier 3, either by getting the plugin to tier 2 or by sunsetting it.
For example, a plugin to provide a formatter which has itself been abandoned has no hope of reaching tier 2, but may be gracefully sunset by only being supported for old versions of GHC, and deleted once those exit our GHC support window.

## Current plugin support tiers

| Plugin                               | Tier | Unsupported GHC versions |
| ------------------------------------ | ---- | ------------------------ |
| ghcide core plugins                  | 1    |                          |
| `hls-call-hierarchy-plugin`          | 1    |                          |
| `hls-code-range-plugin`              | 1    |                          |
| `hls-explicit-imports-plugin`        | 1    |                          |
| `hls-pragmas-plugin`                 | 1    |                          |
| `hls-refactor-plugin`                | 2    |                          |
| `hls-alternate-number-format-plugin` | 2    |                          |
| `hls-cabal-fmt-plugin`               | 2    |                          |
| `hls-cabal-gild-plugin`              | 2    |                          |
| `hls-class-plugin`                   | 2    |                          |
| `hls-change-type-signature-plugin`   | 2    |                          |
| `hls-eval-plugin`                    | 2    |                          |
| `hls-explicit-fixity-plugin`         | 2    |                          |
| `hls-explicit-record-fields-plugin`  | 2    |                          |
| `hls-fourmolu-plugin`                | 2    |                          |
| `hls-gadt-plugin`                    | 2    |                          |
| `hls-hlint-plugin`                   | 2    | 9.10.1                   |
| `hls-module-name-plugin`             | 2    |                          |
| `hls-notes-plugin`                   | 2    |                          |
| `hls-qualify-imported-names-plugin`  | 2    |                          |
| `hls-ormolu-plugin`                  | 2    |                          |
| `hls-rename-plugin`                  | 2    |                          |
| `hls-stylish-haskell-plugin`         | 2    |                          |
| `hls-overloaded-record-dot-plugin`   | 2    |                          |
| `hls-semantic-tokens-plugin`         | 2    |                          |
| `hls-floskell-plugin`                | 3    | 9.10.1, 9.12.2           |
| `hls-stan-plugin`                    | 3    | 9.12.2                   |
| `hls-retrie-plugin`                  | 3    | 9.10.1, 9.12.2           |
| `hls-splice-plugin`                  | 3    | 9.10.1, 9.12.2           |
