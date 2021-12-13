# Changelog for haskell-language-server

## 1.5.1

This is a bug fix release for two regressions found after releasing 1.5.0:

- [#2365](https://github.com/haskell/haskell-language-server/issue/2365): hs-boot files not handled correctly, discovered in the ghc codebase and fixed with [#2377](https://github.com/haskell/haskell-language-server/pull/2377)
- [#2379](https://github.com/haskell/haskell-language-server/issue/2379): `tried to look at linkable for GetModIfaceWithoutLinkable for NormalizedFilePath...` error handling template haskell, fixed with [#2380](https://github.com/haskell/haskell-language-server/pull/2380)

Both quick fixes thanks to @pepeiborra

Also it fixes some long standing bugs in the hlint plugin due to comments being ignored (see [#2366](https://github.com/haskell/haskell-language-server/pull/2366))

### Pull requests merged for 1.5.1

- Fix hls-graph build with embed-files flag
([#2395](https://github.com/haskell/haskell-language-server/pull/2395)) by @pepeiborra
- Prepare 1.5.1
([#2393](https://github.com/haskell/haskell-language-server/pull/2393)) by @jneira
- Revert "Update to latest prettyprinter API (#2352)"
([#2389](https://github.com/haskell/haskell-language-server/pull/2389)) by @pepeiborra
- Add extra logging around build queue
([#2388](https://github.com/haskell/haskell-language-server/pull/2388)) by @pepeiborra
- docs: Fix typo
([#2386](https://github.com/haskell/haskell-language-server/pull/2386)) by @nh2
- Update release instructions
([#2384](https://github.com/haskell/haskell-language-server/pull/2384)) by @jneira
- ghcide: Update dependency on `hls-plugin-api`
([#2382](https://github.com/haskell/haskell-language-server/pull/2382)) by @hololeap
- Fix regression in GhcSessionDeps
([#2380](https://github.com/haskell/haskell-language-server/pull/2380)) by @pepeiborra
- Boot files
([#2377](https://github.com/haskell/haskell-language-server/pull/2377)) by @pepeiborra
- hls-module-name-plugin: Add missing golden file to hackage tarball
([#2374](https://github.com/haskell/haskell-language-server/pull/2374)) by @maralorn
- hls-explicit-imports-plugin: Add golden files to hackage tarball
([#2373](https://github.com/haskell/haskell-language-server/pull/2373)) by @maralorn
- Update ghcide dependency for various plugins
([#2368](https://github.com/haskell/haskell-language-server/pull/2368)) by @hololeap
- Fix several hlint issues related with the use of parsed module without comments
([#2366](https://github.com/haskell/haskell-language-server/pull/2366)) by @jneira

## 1.5.0

Time for another hls release:

- @pepeiborra has done an epic work to improve performance, redefining some of the core pieces of HLS
  - You can take an overall look to improvements in [these slides](https://drive.google.com/file/d/16FpmiHXX_rd2gAf5XVgWAIr4kg-AkUqX/view)
- We have fourmolu support for ghc-9.0.1 thanks to @georgefst
- We have got improvements over import suggestions thanks to @yoshitsugu and @alexnaspo
- Completions also has been improved in general thanks to @pepeiborra
- There have been lot of documentation updates by several contributors, thanks also to all of you
- In this release we still don't have full support for all plugins and ghc-9.0.1
  - Missing plugins for ghc-9.0.1 are: hls-class-plugin, hls-tactics-plugin (wingman), hls-brittany-plugin and hls-stylish-haskell-plugin

### Deprecation notice for 1.5.0

- As we noted in the previous release we have dropped support for ghc versions 8.6.4, 8.10.2, 8.10.3, 8.10.4 in *this release*
- We will drop support for ghc versions 8.10.5 and 8.8.3 *after this release*
- The advise is upgrade ghc to the last minor version: 8.6.5, 8.8.4 or 8.10.7
- You can read more about ghc deprecation policy and schedule [here](https://haskell-language-server.readthedocs.io/en/latest/supported-versions.html)

### Pull requests merged for 1.5.0

- Prepare 1.5.0
([#2361](https://github.com/haskell/haskell-language-server/pull/2361)) by @jneira
- More completion fixes
([#2354](https://github.com/haskell/haskell-language-server/pull/2354)) by @pepeiborra
- Update to latest prettyprinter API
([#2352](https://github.com/haskell/haskell-language-server/pull/2352)) by @fendor
- Use hackage version of czipwith
([#2346](https://github.com/haskell/haskell-language-server/pull/2346)) by @jneira
- Show build graph statistics in ghcide-bench
([#2343](https://github.com/haskell/haskell-language-server/pull/2343)) by @pepeiborra
- contributing: add implicit-hie gen-hie > hie.yaml note
([#2341](https://github.com/haskell/haskell-language-server/pull/2341)) by @Anton-Latukha
- add dependabot: add Actions CI merge requests automation
([#2339](https://github.com/haskell/haskell-language-server/pull/2339)) by @Anton-Latukha
- Skip parsing without haddock for above GHC9.0
([#2338](https://github.com/haskell/haskell-language-server/pull/2338)) by @yoshitsugu
- Give unique names to post-jobs
([#2337](https://github.com/haskell/haskell-language-server/pull/2337)) by @jneira
- Cancel prev runs for bench and nix
([#2335](https://github.com/haskell/haskell-language-server/pull/2335)) by @jneira
- Trace diagnostics
([#2333](https://github.com/haskell/haskell-language-server/pull/2333)) by @pepeiborra
- Include sortText in completions and improve suggestions
([#2332](https://github.com/haskell/haskell-language-server/pull/2332)) by @pepeiborra
- Not suggest exported imports
([#2329](https://github.com/haskell/haskell-language-server/pull/2329)) by @yoshitsugu
- Update troubleshooting section
([#2326](https://github.com/haskell/haskell-language-server/pull/2326)) by @jneira
- Remove automatic comment to [skip circleci]
([#2325](https://github.com/haskell/haskell-language-server/pull/2325)) by @jneira
- Add README.md in install/ subproject
([#2324](https://github.com/haskell/haskell-language-server/pull/2324)) by @sir4ur0n
- Improve the performance of GetModIfaceFromDisk in large repos and delete GetDependencies
([#2323](https://github.com/haskell/haskell-language-server/pull/2323)) by @pepeiborra
- Add support for install hls from hackage using ghc 9.0.1
([#2322](https://github.com/haskell/haskell-language-server/pull/2322)) by @jneira
- Rename hlint test data files and add regression tests
([#2321](https://github.com/haskell/haskell-language-server/pull/2321)) by @jneira
- Suggest hiding imports when local definition exists
([#2320](https://github.com/haskell/haskell-language-server/pull/2320)) by @yoshitsugu
- Improve trace readability
([#2319](https://github.com/haskell/haskell-language-server/pull/2319)) by @pepeiborra
- Sir4ur0n/doc/cleanup hie
([#2311](https://github.com/haskell/haskell-language-server/pull/2311)) by @sir4ur0n
- Add option to effectively cancel prev runs
([#2310](https://github.com/haskell/haskell-language-server/pull/2310)) by @jneira
- Separate features from demos
([#2307](https://github.com/haskell/haskell-language-server/pull/2307)) by @jneira
- Prevent Tactics hover provider from blocking at startup
([#2306](https://github.com/haskell/haskell-language-server/pull/2306)) by @pepeiborra
- Fix defaultIdeOptions to use the initial config settings
([#2302](https://github.com/haskell/haskell-language-server/pull/2302)) by @pepeiborra
- Use new queue rules for mergify bot
([#2301](https://github.com/haskell/haskell-language-server/pull/2301)) by @jneira
- Fix reverse dep. tracking for alwaysRerun rules
([#2298](https://github.com/haskell/haskell-language-server/pull/2298)) by @pepeiborra
- Reorganize github workflows and use specific label [skip circleci]
([#2297](https://github.com/haskell/haskell-language-server/pull/2297)) by @jneira
- Enable func-test suite for windows
([#2296](https://github.com/haskell/haskell-language-server/pull/2296)) by @jneira
- Generate linkables in the Eval plugin
([#2295](https://github.com/haskell/haskell-language-server/pull/2295)) by @pepeiborra
- [hls-graph] clean up databaseDirtySet
([#2294](https://github.com/haskell/haskell-language-server/pull/2294)) by @pepeiborra
- Update link to supported platforms by ghcup
([#2293](https://github.com/haskell/haskell-language-server/pull/2293)) by @chshersh
- Make circleci honour [skip ci] wherever is placed in the pr info (title, description)
([#2289](https://github.com/haskell/haskell-language-server/pull/2289)) by @jneira
- Note in the install script that listed ghcs are the supported ones
([#2286](https://github.com/haskell/haskell-language-server/pull/2286)) by @jneira
- Move hlint tests to its own package (and other clean ups)
([#2284](https://github.com/haskell/haskell-language-server/pull/2284)) by @jneira
- Trace rebuilds
([#2283](https://github.com/haskell/haskell-language-server/pull/2283)) by @pepeiborra
- Fix excessive interface recompilation caused by the Tactics plugin
([#2282](https://github.com/haskell/haskell-language-server/pull/2282)) by @pepeiborra
- Preserve dirty set and add dirtiness assertion
([#2279](https://github.com/haskell/haskell-language-server/pull/2279)) by @pepeiborra
- Ignore null WatchedFile events
([#2278](https://github.com/haskell/haskell-language-server/pull/2278)) by @pepeiborra
- Trace log events and fix ghcide logger
([#2277](https://github.com/haskell/haskell-language-server/pull/2277)) by @pepeiborra
- Point to GitHub from Contributing.md
([#2275](https://github.com/haskell/haskell-language-server/pull/2275)) by @georgefst
- installation.md: add Fedora copr repo
([#2274](https://github.com/haskell/haskell-language-server/pull/2274)) by @juhp
- avoid double rebuilds for FOIs
([#2266](https://github.com/haskell/haskell-language-server/pull/2266)) by @pepeiborra
- Update installation on ArchLinux - new package
([#2265](https://github.com/haskell/haskell-language-server/pull/2265)) by @marcin-rzeznicki
- Garbage collection of dirty keys
([#2263](https://github.com/haskell/haskell-language-server/pull/2263)) by @pepeiborra
- Add lsp-mode links
([#2260](https://github.com/haskell/haskell-language-server/pull/2260)) by @jneira
- Add more features and demos in docs
([#2257](https://github.com/haskell/haskell-language-server/pull/2257)) by @jneira
- Add nix installation section
([#2256](https://github.com/haskell/haskell-language-server/pull/2256)) by @jneira
- Bump Fourmolu to 0.4
([#2254](https://github.com/haskell/haskell-language-server/pull/2254)) by @georgefst
- Remove custom version of operational
([#2249](https://github.com/haskell/haskell-language-server/pull/2249)) by @jneira
- Generate custom source tarball
([#2248](https://github.com/haskell/haskell-language-server/pull/2248)) by @jneira
- Enable the ghcide test plugin in HLS test suites
([#2243](https://github.com/haskell/haskell-language-server/pull/2243)) by @pepeiborra
- Partial sort of fuzzy filtering results
([#2240](https://github.com/haskell/haskell-language-server/pull/2240)) by @pepeiborra
- Fix build with fbghc
([#2234](https://github.com/haskell/haskell-language-server/pull/2234)) by @pepeiborra
- Tweaks to GHC support docs
([#2232](https://github.com/haskell/haskell-language-server/pull/2232)) by @michaelpj
- Add ghc deprecation policy to documentation
([#2231](https://github.com/haskell/haskell-language-server/pull/2231)) by @jneira
- Add ghcup compile option
([#2230](https://github.com/haskell/haskell-language-server/pull/2230)) by @jneira
- Parallel fuzzy filtering
([#2225](https://github.com/haskell/haskell-language-server/pull/2225)) by @pepeiborra
- Revert "Inline Text.Fuzzy to add INLINABLE pragmas"
([#2223](https://github.com/haskell/haskell-language-server/pull/2223)) by @pepeiborra
- feat(flake): expose hie-bios
([#2221](https://github.com/haskell/haskell-language-server/pull/2221)) by @teto
- flake: remove the 'follows' directive
([#2218](https://github.com/haskell/haskell-language-server/pull/2218)) by @teto
- Return completions lazily for massive savings
([#2217](https://github.com/haskell/haskell-language-server/pull/2217)) by @pepeiborra
- Inline Text.Fuzzy to add INLINABLE pragmas
([#2215](https://github.com/haskell/haskell-language-server/pull/2215)) by @pepeiborra
- Add chat on irc badge
([#2214](https://github.com/haskell/haskell-language-server/pull/2214)) by @jneira
- ghcide: Add flags to toggle building each executable
([#2212](https://github.com/haskell/haskell-language-server/pull/2212)) by @hololeap
- Add matrix haskell-tooling channel
([#2210](https://github.com/haskell/haskell-language-server/pull/2210)) by @jneira
- Relax upper bounds over ormolu and stylish-haskell
([#2207](https://github.com/haskell/haskell-language-server/pull/2207)) by @jneira
- Add missing config options in documentation
([#2203](https://github.com/haskell/haskell-language-server/pull/2203)) by @jneira
- Add gitlab CI
([#2200](https://github.com/haskell/haskell-language-server/pull/2200)) by @hasufell
- Apply workaround for 8.8.4 and windows to enable it in ci
([#2199](https://github.com/haskell/haskell-language-server/pull/2199)) by @jneira
- Drop ghc support for 8.6.4, 8.10.2, 8.10.3, 8.10.4
([#2197](https://github.com/haskell/haskell-language-server/pull/2197)) by @jneira
- Consider all root paths when suggesting module name change.
([#2195](https://github.com/haskell/haskell-language-server/pull/2195)) by @cdsmith
- enable completions of local imports
([#2190](https://github.com/haskell/haskell-language-server/pull/2190)) by @alexnaspo
- Drop ghc-api-compat from dependency closure
([#2128](https://github.com/haskell/haskell-language-server/pull/2128)) by @fendor
- Reimplement shake (continued)
([#2060](https://github.com/haskell/haskell-language-server/pull/2060)) by @pepeiborra

## 1.4.0

After a month of vacation a new hls release has arrived:

- Support for ghc 8.10.6 and 8.10.7
- The ormolu formatter plugin works with ghc 9.0.1
- *Call hierarchy plugin has been improved* thanks to @July541:
  - Add call from type signature
  - Add call from a function pattern
  - Go to typeclass instance directly
- As usual @isovector has been busy improving wingman plugin:
  - New "intro and destruct" code action
  - Streaming tactic solutions: when Wingman times outs, it can still pick the best solution it found
  - Let-bindings in metattactics: allows you to bind variables in tactic metaprogram
  - Several bug fixes
- We have new docs thanks to @michaelpj: <https://haskell-language-server.readthedocs.io>
- Now you can ask the executable for included plugins with: `haskell-language-server --list-plugins`
- There are several bug fixes and features you can found in the merged pull requests list

### DEPRECATION NOTICE

- *After* this release we will drop support for ghc versions 8.6.4, 8.10.3 and 8.10.4
  - The advise is upgrade ghc to the last minor version: 8.6.5 or 8.10.7
  - Take a look to [this issue](https://github.com/haskell/haskell-language-server/issues/2168) for more details

### Pull requests merged for 1.4.0

- Prepare 1.4.0
([#2182](https://github.com/haskell/haskell-language-server/pull/2182)) by @jneira
- Update flake to fix nix builds
([#2188](https://github.com/haskell/haskell-language-server/pull/2188)) by @jneira
- Completions for project identifiers
([#2187](https://github.com/haskell/haskell-language-server/pull/2187)) by @pepeiborra
- Wingman: Don't clobber where clauses
([#2184](https://github.com/haskell/haskell-language-server/pull/2184)) by @isovector
- Add rerun workflow
([#2181](https://github.com/haskell/haskell-language-server/pull/2181)) by @jneira
- Bump up shake-bench version
([#2178](https://github.com/haskell/haskell-language-server/pull/2178)) by @jneira
- Fix hackage release
([#2177](https://github.com/haskell/haskell-language-server/pull/2177)) by @jneira
- Use maxBound of uinteger not Int.
([#2169](https://github.com/haskell/haskell-language-server/pull/2169)) by @pranaysashank
- enable the PR gitpod badge and drop the label
([#2167](https://github.com/haskell/haskell-language-server/pull/2167)) by @pepeiborra
- Plugin in config files
([#2166](https://github.com/haskell/haskell-language-server/pull/2166)) by @jneira
- Complete contributing guide
([#2165](https://github.com/haskell/haskell-language-server/pull/2165)) by @jneira
- Wingman: Add "New Unification Variable" helper
([#2164](https://github.com/haskell/haskell-language-server/pull/2164)) by @isovector
- Semiautomatic hackage releases
([#2163](https://github.com/haskell/haskell-language-server/pull/2163)) by @jneira
- Improve incoming call for typeclass and type family instance
([#2162](https://github.com/haskell/haskell-language-server/pull/2162)) by @July541
- Add a Gitpod descriptor
([#2161](https://github.com/haskell/haskell-language-server/pull/2161)) by @pepeiborra
- Wingman: Let-bindings in metatactics
([#2160](https://github.com/haskell/haskell-language-server/pull/2160)) by @isovector
- Update nix flake
([#2159](https://github.com/haskell/haskell-language-server/pull/2159)) by @lf-
- Add ghc-8.10.7 to release build
([#2158](https://github.com/haskell/haskell-language-server/pull/2158)) by @jneira
- Reduce duplication in pragma tests
([#2157](https://github.com/haskell/haskell-language-server/pull/2157)) by @nini-faroux
- Remove ghc-api source snapshot
([#2156](https://github.com/haskell/haskell-language-server/pull/2156)) by @pepeiborra
- Create a citation
([#2155](https://github.com/haskell/haskell-language-server/pull/2155)) by @ndmitchell
- Disable window job for ghc-8.10.2
([#2154](https://github.com/haskell/haskell-language-server/pull/2154)) by @jneira
- Auto complete definitions within imports
([#2152](https://github.com/haskell/haskell-language-server/pull/2152)) by @alexnaspo
- Filter code actions based on prefix, not equality
([#2146](https://github.com/haskell/haskell-language-server/pull/2146)) by @michaelpj
- perform a GC before find resolution
([#2144](https://github.com/haskell/haskell-language-server/pull/2144)) by @pepeiborra
- case sensitive language pragmas fix
([#2142](https://github.com/haskell/haskell-language-server/pull/2142)) by @alexnaspo
- Add ghc-8.10.7 support
([#2141](https://github.com/haskell/haskell-language-server/pull/2141)) by @jneira
- List all available plugins
([#2139](https://github.com/haskell/haskell-language-server/pull/2139)) by @July541
- update LTS for GHC 8.10.6
([#2138](https://github.com/haskell/haskell-language-server/pull/2138)) by @peterbecich
- fix GitHub Actions badges
([#2135](https://github.com/haskell/haskell-language-server/pull/2135)) by @peterbecich
- Move pragmas completion to pragmas plugin
([#2134](https://github.com/haskell/haskell-language-server/pull/2134)) by @alexnaspo
- Update ghc-9.0.1 support
([#2131](https://github.com/haskell/haskell-language-server/pull/2131)) by @jneira
- Support call hierarchy on pattern matching
([#2129](https://github.com/haskell/haskell-language-server/pull/2129)) by @July541
- GHCIDE_BUILD_PROFILING env var
([#2125](https://github.com/haskell/haskell-language-server/pull/2125)) by @pepeiborra
- [ghcide] support -d cli switch
([#2124](https://github.com/haskell/haskell-language-server/pull/2124)) by @pepeiborra
- don't crash when an unused operator import ends in `.`
([#2123](https://github.com/haskell/haskell-language-server/pull/2123)) by @tscholak
- [benchmarks] Fix edit and "after edit" experiments
([#2122](https://github.com/haskell/haskell-language-server/pull/2122)) by @pepeiborra
- Add fix for correct placement of import (#2100)
([#2116](https://github.com/haskell/haskell-language-server/pull/2116)) by @nini-faroux
- Support for ghc-8.10.6
([#2109](https://github.com/haskell/haskell-language-server/pull/2109)) by @jneira
- New rename plugin implementation
([#2108](https://github.com/haskell/haskell-language-server/pull/2108)) by @OliverMadine
- [ghcide-bench] Support extra args in examples
([#2107](https://github.com/haskell/haskell-language-server/pull/2107)) by @pepeiborra
- Fix filepath identity in cradle dependencies when using reactive change tracking
([#2106](https://github.com/haskell/haskell-language-server/pull/2106)) by @pepeiborra
- [ghcide-bench] preserve threading details in eventlogs
([#2105](https://github.com/haskell/haskell-language-server/pull/2105)) by @pepeiborra
- [ghcide-bench] fix edit experiment
([#2104](https://github.com/haskell/haskell-language-server/pull/2104)) by @pepeiborra
([#2102](https://github.com/haskell/haskell-language-server/pull/2102)) by @isovector
- reduce allow-newer entries for shake-bench
([#2101](https://github.com/haskell/haskell-language-server/pull/2101)) by @pepeiborra
- Wingman: Don't count it as using a term if you only destruct it
([#2099](https://github.com/haskell/haskell-language-server/pull/2099)) by @isovector
- Clean cabal project
([#2097](https://github.com/haskell/haskell-language-server/pull/2097)) by @jneira
- Wingman: New AbstractLSP interface
([#2094](https://github.com/haskell/haskell-language-server/pull/2094)) by @isovector
- Add badge with github release
([#2093](https://github.com/haskell/haskell-language-server/pull/2093)) by @jneira
- Add a bit more prose and some links to the README
([#2090](https://github.com/haskell/haskell-language-server/pull/2090)) by @michaelpj
- Enable tests for ormolu plugin
([#2086](https://github.com/haskell/haskell-language-server/pull/2086)) by @felixonmars
- Allow ormolu 0.2 and fix compatibility with GHC 9
([#2084](https://github.com/haskell/haskell-language-server/pull/2084)) by @felixonmars
- Add initial sphinx doc site for RTD
([#2083](https://github.com/haskell/haskell-language-server/pull/2083)) by @michaelpj
- Amend fix for correct placement of file header pragmas (#1958)
([#2078](https://github.com/haskell/haskell-language-server/pull/2078)) by @nini-faroux
- Wingman: "Intro and destruct" code action
([#2077](https://github.com/haskell/haskell-language-server/pull/2077)) by @isovector
- Support call hierarchy on type signature & add plugin to generic config  & docs
([#2072](https://github.com/haskell/haskell-language-server/pull/2072)) by @July541
- Update nix flake
([#2065](https://github.com/haskell/haskell-language-server/pull/2065)) by @berberman
- Include sponsorship section
([#2063](https://github.com/haskell/haskell-language-server/pull/2063)) by @jneira
- Add more communication channels
([#2062](https://github.com/haskell/haskell-language-server/pull/2062)) by @jneira
- Don't suggest disabling type errors
([#2061](https://github.com/haskell/haskell-language-server/pull/2061)) by @anka-213
- Build with lsp 1.2.0.1
([#2059](https://github.com/haskell/haskell-language-server/pull/2059)) by @pepeiborra
- Remove HIE_CACHE from circleci cache key
([#2050](https://github.com/haskell/haskell-language-server/pull/2050)) by @jneira
- [#1958] Fix placement of language pragmas
([#2043](https://github.com/haskell/haskell-language-server/pull/2043)) by @nini-faroux
- [#2005] Fix Formatting When Brittany Returns Warnings
([#2036](https://github.com/haskell/haskell-language-server/pull/2036)) by @prikhi

## 1.3.0

2021 July release of HLS arrives! This release includes binaries for GHC 9.0.1
and some new interesting features. Here is the brief summary of changes:

- Binaries for GHC 9.0.1 are added by @anka-213.
- Call hierarchy plugin is added, contributed by @July541.
  ![hierarchy](https://user-images.githubusercontent.com/12473268/127550041-094151a6-be7b-484a-bb82-c61f326ca503.gif)
- Now completions work with definitions from non-imported modules, thanks to @pepeiborra.
  ![completion](https://user-images.githubusercontent.com/12473268/127543694-718ae043-38f2-4fb0-be71-317f5f93b443.gif)
- Eval plugin
  - The plugin supports GHC 9.0.1, thanks to @berberman.
  - `:info` command is added by @akrmn.
  - The plugin uses the same default language as GHCi with @fmehta's patch.
- Wingman, where most changes owing to @isovector
  - Wingman no longer changes the fixity of function definitions.
  - Wingman now gives unique names to the holes it generates.
  - Wingman's ability to reason about polymorphic and GADT types is significantly improved.
  - Wingman no longer suggests homomorphic destructs when the codomain is larger than the domain.
  - "Complete case constructors" action supports empty lambda cases.
  - Wingman now gives a warning if it ran out of gas during "attempt to fill hole".
  - Metaprogramming for Wingman has been improved with symbolic-name support and the `pointwise` combinator.
  - An option to enable/disable Wingman's proof state styling is added.
  - Hole fit suggestions are now disabled for performance reasons when using Wingman.
- Hovering on a name displays the package where the name is defined, contributed by @berberman.
  ![hover](https://user-images.githubusercontent.com/12473268/127550516-acc1f1b4-bad7-44fd-99a0-a174ce9ac909.gif)

### Pull requests merged for 1.3.0

- Wingman: Properly destruct forall-quantified types
([#2049](https://github.com/haskell/haskell-language-server/pull/2049)) by @isovector
- Remove .stack-work from circleci cache
([#2044](https://github.com/haskell/haskell-language-server/pull/2044)) by @jneira
- Completions from non-imported modules
([#2040](https://github.com/haskell/haskell-language-server/pull/2040)) by @pepeiborra
- Wingman: Low gas warning
([#2038](https://github.com/haskell/haskell-language-server/pull/2038)) by @isovector
- Enable dynamic linking in stack builds
([#2031](https://github.com/haskell/haskell-language-server/pull/2031)) by @pepeiborra
- Fix nix flake
([#2030](https://github.com/haskell/haskell-language-server/pull/2030)) by @Avi-D-coder
- Tie plugins' pluginModifyDynflags to their enabled state
([#2029](https://github.com/haskell/haskell-language-server/pull/2029)) by @isovector
- Add benchmarks for hole fits
([#2027](https://github.com/haskell/haskell-language-server/pull/2027)) by @pepeiborra
- fix a typo
([#2024](https://github.com/haskell/haskell-language-server/pull/2024)) by @cdsmith
- Upgrade to refinery-0.4.0.0
([#2021](https://github.com/haskell/haskell-language-server/pull/2021)) by @isovector
- Use implicit-hie-cradle-0.3.0.5
([#2020](https://github.com/haskell/haskell-language-server/pull/2020)) by @jneira
- Disable hls tests for win and ghc-9.0.1
([#2018](https://github.com/haskell/haskell-language-server/pull/2018)) by @jneira
- Use operational master commit to fix build for ghc-9.0.1
([#2017](https://github.com/haskell/haskell-language-server/pull/2017)) by @jneira
- Fix Wingman dependency on extra
([#2007](https://github.com/haskell/haskell-language-server/pull/2007)) by @pepeiborra
- Add GHC 9.2 support for hie-compat
([#2003](https://github.com/haskell/haskell-language-server/pull/2003)) by @fendor
- Enable tests for ghc 9 and promote `ghcVersion` check
([#2001](https://github.com/haskell/haskell-language-server/pull/2001)) by @jneira
- Allow HLS plugins to declare cli commands
([#1999](https://github.com/haskell/haskell-language-server/pull/1999)) by @pepeiborra
- Remove >= from cabal-version
([#1998](https://github.com/haskell/haskell-language-server/pull/1998)) by @felixonmars
- Eval plugin: support ghc 9.0.1
([#1997](https://github.com/haskell/haskell-language-server/pull/1997)) by @berberman
- Maximize sharing of NormalizedFilePath values in getLocatedImports
([#1996](https://github.com/haskell/haskell-language-server/pull/1996)) by @pepeiborra
- nix: add support for ghc 9.0.1
([#1995](https://github.com/haskell/haskell-language-server/pull/1995)) by @berberman
- Warn GHC 9 Compatibility to LSP Client
([#1992](https://github.com/haskell/haskell-language-server/pull/1992)) by @konn
- Update nix to GHC 8.10.5
([#1991](https://github.com/haskell/haskell-language-server/pull/1991)) by @berberman
- Initialize ExportsMap using hiedb exports
([#1989](https://github.com/haskell/haskell-language-server/pull/1989)) by @pepeiborra
- Wingman: add emacs example config to Readme
([#1988](https://github.com/haskell/haskell-language-server/pull/1988)) by @stuebinm
- relax megaparsec constraint in hls-tactics-plugin
([#1986](https://github.com/haskell/haskell-language-server/pull/1986)) by @pepeiborra
- follow change in lsp-types
([#1985](https://github.com/haskell/haskell-language-server/pull/1985)) by @pepeiborra
- Don't suggest import an unnecessary data constructor.
([#1984](https://github.com/haskell/haskell-language-server/pull/1984)) by @peterwicksstringfield
- Enable hyphenation embedding
([#1979](https://github.com/haskell/haskell-language-server/pull/1979)) by @isovector
- Fix nix.yaml
([#1974](https://github.com/haskell/haskell-language-server/pull/1974)) by @isovector
- Add windows to ghcup artifacts and generate sha256 sums
([#1970](https://github.com/haskell/haskell-language-server/pull/1970)) by @jneira
- Wingman: Ensure homomorphic destruct covers all constructors in the domain
([#1968](https://github.com/haskell/haskell-language-server/pull/1968)) by @isovector
- Wingman: Add the correct file offset to metaprogram parse errors
([#1967](https://github.com/haskell/haskell-language-server/pull/1967)) by @isovector
- Wingman: Config option to suppress proofstate styling
([#1966](https://github.com/haskell/haskell-language-server/pull/1966)) by @isovector
- Wingman: Don't wildify vars when running beginMetaprogram
([#1963](https://github.com/haskell/haskell-language-server/pull/1963)) by @isovector
- Wingman: Don't suggest empty case lenses for case exprs with no data cons
([#1962](https://github.com/haskell/haskell-language-server/pull/1962)) by @isovector
- Wingman: Don't introduce too many variables
([#1961](https://github.com/haskell/haskell-language-server/pull/1961)) by @isovector
- Wingman: Code lens for empty lambda case
([#1956](https://github.com/haskell/haskell-language-server/pull/1956)) by @isovector
- Call hierarchy support
([#1955](https://github.com/haskell/haskell-language-server/pull/1955)) by @July541
- Bugfix type signature lenses / code actions for pattern synonyms.
([#1952](https://github.com/haskell/haskell-language-server/pull/1952)) by @peterwicksstringfield
- Add :info command in Eval plugin
([#1948](https://github.com/haskell/haskell-language-server/pull/1948)) by @akrmn
- avoid holding onto the hie bytestring when indexing
([#1947](https://github.com/haskell/haskell-language-server/pull/1947)) by @pepeiborra
- Wingman: Make getCurrentDefinitions return polymorphic types
([#1945](https://github.com/haskell/haskell-language-server/pull/1945)) by @isovector
- Wingman: Tactical support for deep recursion
([#1944](https://github.com/haskell/haskell-language-server/pull/1944)) by @isovector
- Properly scope GADT equality evidence in the judgment
([#1942](https://github.com/haskell/haskell-language-server/pull/1942)) by @isovector
- Add ghc-9.0.1 to the build release script
([#1940](https://github.com/haskell/haskell-language-server/pull/1940)) by @anka-213
- Cata tactic should generalize let and ensure unifiability
([#1938](https://github.com/haskell/haskell-language-server/pull/1938)) by @isovector
- Include chocolatey hls package
([#1936](https://github.com/haskell/haskell-language-server/pull/1936)) by @jneira
- Mention ghcup and warning about updating artifacts
([#1935](https://github.com/haskell/haskell-language-server/pull/1935)) by @jneira
- Remove ghc-8.8.2
([#1934](https://github.com/haskell/haskell-language-server/pull/1934)) by @jneira
- Workaround for GHC 8.10.5 on macOS
([#1931](https://github.com/haskell/haskell-language-server/pull/1931)) by @konn
- Add manual upload instructions
([#1930](https://github.com/haskell/haskell-language-server/pull/1930)) by @jneira
- Perform name lookup directly in TacticsM
([#1924](https://github.com/haskell/haskell-language-server/pull/1924)) by @isovector
- Include testdata in hls-refine-imports-plugin.cabal (backport #1922)
([#1923](https://github.com/haskell/haskell-language-server/pull/1923)) by @mergify[bot]
- Include testdata in hls-refine-imports-plugin.cabal
([#1922](https://github.com/haskell/haskell-language-server/pull/1922)) by @felixonmars
- Add pointwise command to the metaprogram parser
([#1921](https://github.com/haskell/haskell-language-server/pull/1921)) by @isovector
- Allow symbol identifiers in tactics
([#1920](https://github.com/haskell/haskell-language-server/pull/1920)) by @isovector
- Fall back to hiedb for invalid srcspan paths
([#1918](https://github.com/haskell/haskell-language-server/pull/1918)) by @pepeiborra
- Disable hole fit suggestions when running Wingman
([#1873](https://github.com/haskell/haskell-language-server/pull/1873)) by @isovector
- Wingman: maintain user-defined fixity for definitions
([#1697](https://github.com/haskell/haskell-language-server/pull/1697)) by @isovector
- Display package names of external libraries on hover
([#1626](https://github.com/haskell/haskell-language-server/pull/1626)) by @berberman
- Make the eval plugin use the same default language extensions as ghci.
([#1596](https://github.com/haskell/haskell-language-server/pull/1596)) by @fmehta

## 1.2.0

We have finally released a new version of Haskell Language Server!
Thanks for all contributors, many bugs has been fixed, and many features has landed.
Here are the summary of changes:

- Basic support for GHC 9.0.1 is added.
  It does not support all plugins yet, but core GHCIDE features will work. For the detailed information that which plugins work, please refer [this list](https://github.com/haskell/haskell-language-server/issues/297#issuecomment-855522891).
- Support for GHC 8.10.5 is added.
  Note that macOS version is unfortunately not included in this release because of [a GHC issue with `network` package](https://gitlab.haskell.org/ghc/ghc/-/issues/19968).
- HLS wrapper and GHCIDE session loader uses the same logic with implicit-hie.
  This fixes [a build issue](https://github.com/haskell/haskell-language-server/issues/1782) of a stack project with implicit `hie.yaml` .
- Wingman plugin has added numerous features and fixed many bugs:
  - It now supports tactic metaprogramming!
    For list of commands, see [this document](https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-tactics-plugin/COMMANDS.md#wingman-metaprogram-command-reference).
    ![https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-tactics-plugin/COMMANDS.md#wingman-metaprogram-command-reference](https://user-images.githubusercontent.com/307223/118190278-bdf24f80-b3f7-11eb-8838-b08a2582d7f1.gif)
  - "Refine hole" and "Split all function arguments" code actions are publicly opened.
  - "Empty case split" code lens is added.
  - The name generator is fixed [to avoid dangerous summon rituals](https://github.com/haskell/haskell-language-server/pull/1760).
  - Many bugs related to type families and GADTs are fixed.
- We support [nix flake](https://nixos.wiki/wiki/Flakes), an upcoming way to manage dependencies in nix.
- Every plugin (other than example plugins) now lives in its own package.

### Pull requests merged for 1.2.0

- Cleanup stack build output in circleci
([#1905](https://github.com/haskell/haskell-language-server/pull/1905)) by @jhrcek
- Remove FeatureSet
([#1902](https://github.com/haskell/haskell-language-server/pull/1902)) by @isovector
- Correct a typo in ConfigUtils.hs
([#1900](https://github.com/haskell/haskell-language-server/pull/1900)) by @felixonmars
- Add GHC 8.10.5 support
([#1899](https://github.com/haskell/haskell-language-server/pull/1899)) by @Ailrun
- Fix getCurrentDirectory calls in ghcide
([#1897](https://github.com/haskell/haskell-language-server/pull/1897)) by @pepeiborra
- Wingman: FIx evidence when using GADT constructors
([#1889](https://github.com/haskell/haskell-language-server/pull/1889)) by @isovector
- [explicit-imports] Take in a predicate to filter modules
([#1888](https://github.com/haskell/haskell-language-server/pull/1888)) by @pepeiborra
- Fix unification pertaining to evidence
([#1885](https://github.com/haskell/haskell-language-server/pull/1885)) by @isovector
- Let Wingman peek through type families
([#1881](https://github.com/haskell/haskell-language-server/pull/1881)) by @isovector
- Use file watches for all workspace files
([#1880](https://github.com/haskell/haskell-language-server/pull/1880)) by @pepeiborra
- Update IRC details in README.md
([#1877](https://github.com/haskell/haskell-language-server/pull/1877)) by @fendor
- Fix nix build for #1858
([#1870](https://github.com/haskell/haskell-language-server/pull/1870)) by @berberman
- Wingman metaprogram command documentation
([#1867](https://github.com/haskell/haskell-language-server/pull/1867)) by @isovector
- Catamorphism and collapse tactics
([#1865](https://github.com/haskell/haskell-language-server/pull/1865)) by @isovector
- Fix condition of nix build job
([#1864](https://github.com/haskell/haskell-language-server/pull/1864)) by @berberman
- Technology preview: Keep track of changes to minimize rebuilds
([#1862](https://github.com/haskell/haskell-language-server/pull/1862)) by @pepeiborra
- Trace more Shake evaluation details
([#1861](https://github.com/haskell/haskell-language-server/pull/1861)) by @pepeiborra
- No need to delete the same key twice
([#1860](https://github.com/haskell/haskell-language-server/pull/1860)) by @pepeiborra
- Use cabal-install if nix is failing in CI
([#1859](https://github.com/haskell/haskell-language-server/pull/1859)) by @berberman
- Use last apply-refact and several stack.yaml updates
([#1858](https://github.com/haskell/haskell-language-server/pull/1858)) by @jneira
- Split ghcide actions into different descriptors
([#1857](https://github.com/haskell/haskell-language-server/pull/1857)) by @berberman
- Allow module-local and imported functions in Wingman metaprograms
([#1856](https://github.com/haskell/haskell-language-server/pull/1856)) by @isovector
- Update mergify.yml
([#1853](https://github.com/haskell/haskell-language-server/pull/1853)) by @Ailrun
- Fix flake compat
([#1852](https://github.com/haskell/haskell-language-server/pull/1852)) by @berberman
- Fix record layout
([#1851](https://github.com/haskell/haskell-language-server/pull/1851)) by @isovector
- Avoid package-qualified import in Fourmolu plugin
([#1848](https://github.com/haskell/haskell-language-server/pull/1848)) by @georgefst
- Skip cachix jobs if token is unset
([#1845](https://github.com/haskell/haskell-language-server/pull/1845)) by @berberman
- Refine should either do intros or split, not both
([#1842](https://github.com/haskell/haskell-language-server/pull/1842)) by @isovector
- Add hspec upper bound
([#1837](https://github.com/haskell/haskell-language-server/pull/1837)) by @jneira
- Extract last 2 plugins and clean up others
([#1836](https://github.com/haskell/haskell-language-server/pull/1836)) by @Ailrun
- Extract pragmas plugin
([#1833](https://github.com/haskell/haskell-language-server/pull/1833)) by @Ailrun
- Extract floskell plugin as a standalone plugin
([#1829](https://github.com/haskell/haskell-language-server/pull/1829)) by @Ailrun
- nix: refactor with flakes
([#1827](https://github.com/haskell/haskell-language-server/pull/1827)) by @berberman
- Bump up hls-splice-plugin to 1.0.0.2 (backport #1825)
([#1826](https://github.com/haskell/haskell-language-server/pull/1826)) by @mergify[bot]
- Bump up hls-splice-plugin to 1.0.0.2
([#1825](https://github.com/haskell/haskell-language-server/pull/1825)) by @jneira
- Apply formats again
([#1824](https://github.com/haskell/haskell-language-server/pull/1824)) by @Ailrun
- Extract fourmolu plugin into a standalone package
([#1823](https://github.com/haskell/haskell-language-server/pull/1823)) by @gustavoavena
- Ignore filemode in diff
([#1819](https://github.com/haskell/haskell-language-server/pull/1819)) by @Ailrun
- ghc-api cleanups cleanup
([#1816](https://github.com/haskell/haskell-language-server/pull/1816)) by @pepeiborra
- Add a hook for modifying the dynflags from a plugin
([#1814](https://github.com/haskell/haskell-language-server/pull/1814)) by @isovector
- Prepare ghcide release v1.3.0.0
([#1811](https://github.com/haskell/haskell-language-server/pull/1811)) by @pepeiborra
- Remove hls-ghc-x.y from install script and wrapper
([#1805](https://github.com/haskell/haskell-language-server/pull/1805)) by @berberman
- Fix unwanted import refinement
([#1801](https://github.com/haskell/haskell-language-server/pull/1801)) by @rayshih
- Canonicalize hiedb path before comparing
([#1800](https://github.com/haskell/haskell-language-server/pull/1800)) by @pepeiborra
- Pin nix-pre-commit-hooks (backport #1780)
([#1798](https://github.com/haskell/haskell-language-server/pull/1798)) by @mergify[bot]
- Add upper bound to hlint (backport #1795)
([#1797](https://github.com/haskell/haskell-language-server/pull/1797)) by @mergify[bot]
- Add bounds for base in hls-stylish-haskell-plugin (backport #1794)
([#1796](https://github.com/haskell/haskell-language-server/pull/1796)) by @mergify[bot]
- Add upper bound to hlint
([#1795](https://github.com/haskell/haskell-language-server/pull/1795)) by @jneira
- Add bounds for base in hls-stylish-haskell-plugin
([#1794](https://github.com/haskell/haskell-language-server/pull/1794)) by @berberman
- Add bounds for base in hls-test-utils (backport #1791)
([#1793](https://github.com/haskell/haskell-language-server/pull/1793)) by @mergify[bot]
- Replace faulty signature test
([#1792](https://github.com/haskell/haskell-language-server/pull/1792)) by @kderme
- Add bounds for base in hls-test-utils
([#1791](https://github.com/haskell/haskell-language-server/pull/1791)) by @berberman
- Fix backport conflict of refine-import plugin
([#1790](https://github.com/haskell/haskell-language-server/pull/1790)) by @Ailrun
- Fix progress counting
([#1789](https://github.com/haskell/haskell-language-server/pull/1789)) by @pepeiborra
- Loosen dependency bounds (backport #1787)
([#1788](https://github.com/haskell/haskell-language-server/pull/1788)) by @mergify[bot]
- Loosen dependency bounds
([#1787](https://github.com/haskell/haskell-language-server/pull/1787)) by @berberman
- clean up ghc-api pragmas
([#1785](https://github.com/haskell/haskell-language-server/pull/1785)) by @pepeiborra
- Progress reporting improvements
([#1784](https://github.com/haskell/haskell-language-server/pull/1784)) by @pepeiborra
- Unify session loading using implicit-hie
([#1783](https://github.com/haskell/haskell-language-server/pull/1783)) by @fendor
- Pin nix-pre-commit-hooks
([#1780](https://github.com/haskell/haskell-language-server/pull/1780)) by @Ailrun
- Replace the unsafe getmodtime with safe posix calls
([#1778](https://github.com/haskell/haskell-language-server/pull/1778)) by @pepeiborra
- Tactic metaprogramming
([#1776](https://github.com/haskell/haskell-language-server/pull/1776)) by @isovector
- Fix wrong extend import while type constuctor and data constructor have the same name
([#1775](https://github.com/haskell/haskell-language-server/pull/1775)) by @July541
- Add codetriage badge
([#1772](https://github.com/haskell/haskell-language-server/pull/1772)) by @jneira
- Wingman: configurable auto search depth
([#1771](https://github.com/haskell/haskell-language-server/pull/1771)) by @isovector
- Prevent accidental Cthulhu summons
([#1760](https://github.com/haskell/haskell-language-server/pull/1760)) by @isovector
- Delay the Shake session setup until the Initialized handler
([#1754](https://github.com/haskell/haskell-language-server/pull/1754)) by @pepeiborra
- Wrap the Shake functions with newtypes
([#1753](https://github.com/haskell/haskell-language-server/pull/1753)) by @ndmitchell
- Fix reduction depth
([#1751](https://github.com/haskell/haskell-language-server/pull/1751)) by @pepeiborra
- Add hls-graph abstracting over shake
([#1748](https://github.com/haskell/haskell-language-server/pull/1748)) by @ndmitchell
- Explicitly import liftIO if you need it, rather than getting it from Shake
([#1747](https://github.com/haskell/haskell-language-server/pull/1747)) by @ndmitchell
- Tease apart the custom SYB from ExactPrint
([#1746](https://github.com/haskell/haskell-language-server/pull/1746)) by @isovector
- Remove unnecessary Shake dependencies
([#1745](https://github.com/haskell/haskell-language-server/pull/1745)) by @ndmitchell
- Delete an unused import
([#1744](https://github.com/haskell/haskell-language-server/pull/1744)) by @ndmitchell
- Improve vscode extension schema generation
([#1742](https://github.com/haskell/haskell-language-server/pull/1742)) by @berberman
- Fix class method completion
([#1741](https://github.com/haskell/haskell-language-server/pull/1741)) by @July541
- Add heralds to Wingman's use of runAction
([#1740](https://github.com/haskell/haskell-language-server/pull/1740)) by @isovector
- Wingman: case split on punned record fields
([#1739](https://github.com/haskell/haskell-language-server/pull/1739)) by @isovector
- Wingman feature release
([#1735](https://github.com/haskell/haskell-language-server/pull/1735)) by @isovector
- Add haskell-language-server Homebrew installation instructions
([#1734](https://github.com/haskell/haskell-language-server/pull/1734)) by @kret
- Add a "Split using NamedFieldPuns" code action
([#1733](https://github.com/haskell/haskell-language-server/pull/1733)) by @isovector
- Insert pragmas after shebang or to existing pragma list
([#1731](https://github.com/haskell/haskell-language-server/pull/1731)) by @OliverMadine
- Add executable stanza in hls-install.cabal.
([#1730](https://github.com/haskell/haskell-language-server/pull/1730)) by @arrowd
- Add installation instructions for FreeBSD.
([#1729](https://github.com/haskell/haskell-language-server/pull/1729)) by @arrowd
- HLint: Pass options through user config
([#1724](https://github.com/haskell/haskell-language-server/pull/1724)) by @rmehri01
- Prepare ghcide 1.2.0.2 and HLS 1.1.0
([#1722](https://github.com/haskell/haskell-language-server/pull/1722)) by @berberman
- Wingman: Destruct on empty case
([#1721](https://github.com/haskell/haskell-language-server/pull/1721)) by @isovector
- Fix: #1690 - Infix typed holes are now filled using infix notation
([#1708](https://github.com/haskell/haskell-language-server/pull/1708)) by @OliverMadine
- Implement refine imports
([#1686](https://github.com/haskell/haskell-language-server/pull/1686)) by @rayshih
- Ghc 9.0.1 support for ghcide
([#1649](https://github.com/haskell/haskell-language-server/pull/1649)) by @anka-213
- hie-compat: Add basic support for ghc 9.0.1
([#1635](https://github.com/haskell/haskell-language-server/pull/1635)) by @anka-213
- Fix remove constraint
([#1578](https://github.com/haskell/haskell-language-server/pull/1578)) by @kderme
- Limit CodeActions within passed range
([#1442](https://github.com/haskell/haskell-language-server/pull/1442)) by @aufarg

## 1.1.0

Haskell Language Server 1.1.0 has finally come! Many thanks to all contributors -- since the last release, we have merged over 100 PRs!
As always, there are many internal bug fixes and performance improvements in ghcide. Apart from that,

- Wingman gets many enhancements, thanks to @isovector for this epic work!
  - Wingman actions can now be bound to editor hotkeys
  - Experimental support for "jump to next unsolved hole"
  - Improved layout algorithm --- don't reflow instances, or break do-blocks
  - Wingman can now deal with GADTs, rank-n types and pattern synonyms
  - Wingman now respects user-written bindings on the left side of the equals sign
  - Significantly more-natural synthesized code when dealing with newtypes, infix operators, records and strings
  - Improved user experience --- less waiting, and friendly errors for when things go wrong
- hlint plugin not working in some cases gets fixed
- annoying log message "haskell-lsp:incoming message parse error" gets fixed in `lsp-1.2`
- eval plugin now supports `it` variable, like GHCi
- verbose message "No cradle found for ... Proceeding with implicit cradle" is GONE
- type lenses plugin now has its custom config `mode` (enum) [`always`] to control its working mode:
  - `always`: always displays type signature lenses of global bindings
  - `exported`: similar to `always`, but only displays for exported global bindings
  - `diagnostics`: follows diagnostic messages produced by GHC
- top-level LSP option `completionSnippetsOn` and `maxNumberOfProblems` are deprecated
- completions plugin now has its custom config:
  - `autoExtendOn` (boolean) [`true`]: whether to enable auto extending import lists
  - `snippetsOn` (boolean) [`true`]: wheter to enable completion snippets, taking the place of `completionSnippetsOn`
- Wingman has its custom config:
  - `timeout_duration` (integer) [`2`]: the timeout for Wingman actions, in seconds
  - `features` (string) [`""`]: feature set used by Wingman (See [the README of Wingman](https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-tactics-plugin#readme))
  - `max_use_ctor_actions` (integer) [`5`]: maximum number of `Use constructor <x>` code actions that can appear
  - `hole_severity` (enum) [`none`]: the severity to use when showing hole diagnostics
- LSP symbols of typeclass and type families are more appropriate
- test suite of plugins are reorganized, which no longer need to be run with `test-server` executable
- two new packages `hls-test-utils` and `hls-stylish-haskell-plugin` are extracted

This version uses `lsp-1.2.0`, `hls-plugin-api-1.1.0`, and `ghcide-1.2.0.2`.

### Pull requests merged for 1.1.0

- Restore compat. with haddock-library 1.8
([#1717](https://github.com/haskell/haskell-language-server/pull/1717)) by @pepeiborra
- Don't suggest destruct actions for already-destructed terms
([#1715](https://github.com/haskell/haskell-language-server/pull/1715)) by @isovector
- Add keybindings and jump to hole to the Wingman README
([#1712](https://github.com/haskell/haskell-language-server/pull/1712)) by @isovector
- Bracketing for snippet completions
([#1709](https://github.com/haskell/haskell-language-server/pull/1709)) by @OliverMadine
- Prepare ghcide 1.2.0
([#1707](https://github.com/haskell/haskell-language-server/pull/1707)) by @berberman
- Adjust bounds
([#1701](https://github.com/haskell/haskell-language-server/pull/1701)) by @berberman
- Update nix
([#1699](https://github.com/haskell/haskell-language-server/pull/1699)) by @berberman
- Wingman: "Destruct all" only on ADTs
([#1695](https://github.com/haskell/haskell-language-server/pull/1695)) by @isovector
- Fix ghcide and HLS enter lsp mode by default
([#1692](https://github.com/haskell/haskell-language-server/pull/1692)) by @berberman
- Decrease Wingman timeout from 3.3 minutes to 2 seconds (configurable)
([#1688](https://github.com/haskell/haskell-language-server/pull/1688)) by @isovector
- Wrap test suite of tactics plugin into tasty test tree
([#1676](https://github.com/haskell/haskell-language-server/pull/1676)) by @berberman
- Wingman: Use infix notation for operator applications
([#1675](https://github.com/haskell/haskell-language-server/pull/1675)) by @isovector
- Ignore ghcide tests by paths
([#1673](https://github.com/haskell/haskell-language-server/pull/1673)) by @jneira
- Ignore nix job steps by path
([#1672](https://github.com/haskell/haskell-language-server/pull/1672)) by @jneira
- Intelligent derivations of Semigroup and Monoid for Wingman
([#1671](https://github.com/haskell/haskell-language-server/pull/1671)) by @isovector
- optimize ambiguity import suggestions
([#1669](https://github.com/haskell/haskell-language-server/pull/1669)) by @July541
- Replace Barrier with MVar in lsp main
([#1668](https://github.com/haskell/haskell-language-server/pull/1668)) by @berberman
- ghcide - enable ApplicativeDo everywhere
([#1667](https://github.com/haskell/haskell-language-server/pull/1667)) by @pepeiborra
- support custom Ide commands
([#1666](https://github.com/haskell/haskell-language-server/pull/1666)) by @pepeiborra
- Add bounds for Diff
([#1665](https://github.com/haskell/haskell-language-server/pull/1665)) by @berberman
- Update shake bounds of install script
([#1664](https://github.com/haskell/haskell-language-server/pull/1664)) by @berberman
- Avoid creating IsFileOfInterest keys for non workspace files
([#1661](https://github.com/haskell/haskell-language-server/pull/1661)) by @pepeiborra
- additional .gitignore entries
([#1659](https://github.com/haskell/haskell-language-server/pull/1659)) by @pepeiborra
- Skip tracing unless eventlog is enabled
([#1658](https://github.com/haskell/haskell-language-server/pull/1658)) by @pepeiborra
- Fix a wingman bug caused by mismanaged stale data
([#1657](https://github.com/haskell/haskell-language-server/pull/1657)) by @isovector
- Fix ignore paths
([#1656](https://github.com/haskell/haskell-language-server/pull/1656)) by @jneira
- Shut the Shake session on exit, instead of restarting it
([#1655](https://github.com/haskell/haskell-language-server/pull/1655)) by @pepeiborra
- Emit holes as diagnostics
([#1653](https://github.com/haskell/haskell-language-server/pull/1653)) by @isovector
- log exceptions before killing the server
([#1651](https://github.com/haskell/haskell-language-server/pull/1651)) by @pepeiborra
- Do not override custom commands
([#1650](https://github.com/haskell/haskell-language-server/pull/1650)) by @pepeiborra
- Fix importing type operators
([#1644](https://github.com/haskell/haskell-language-server/pull/1644)) by @berberman
- Add haskell-language-server-bin to Arch Linux section
([#1642](https://github.com/haskell/haskell-language-server/pull/1642)) by @marcin-rzeznicki
- Update ISSUE_TEMPLATE.md
([#1640](https://github.com/haskell/haskell-language-server/pull/1640)) by @Ailrun
- Civilized indexing progress reporting
([#1633](https://github.com/haskell/haskell-language-server/pull/1633)) by @pepeiborra
- Update to lsp-1.2
([#1631](https://github.com/haskell/haskell-language-server/pull/1631)) by @wz1000
- Avoid reordering plugins
([#1629](https://github.com/haskell/haskell-language-server/pull/1629)) by @pepeiborra
- Run plugins' test suites with server in the same process
([#1628](https://github.com/haskell/haskell-language-server/pull/1628)) by @berberman
- Remove ignored paths
([#1623](https://github.com/haskell/haskell-language-server/pull/1623)) by @jneira
- Update formatting hooks to not include Wingman
([#1622](https://github.com/haskell/haskell-language-server/pull/1622)) by @Ailrun
- Add CPP Options for Stylish Haskell & Brittany Formatters
([#1620](https://github.com/haskell/haskell-language-server/pull/1620)) by @prikhi
- Use custom config for completions plugin
([#1619](https://github.com/haskell/haskell-language-server/pull/1619)) by @berberman
- Configurable I/O handles
([#1617](https://github.com/haskell/haskell-language-server/pull/1617)) by @pepeiborra
- Add installation instructions for Arch Linux
([#1616](https://github.com/haskell/haskell-language-server/pull/1616)) by @berberman
- Properly pass argFiles into defaultMain
([#1613](https://github.com/haskell/haskell-language-server/pull/1613)) by @mpickering
- Migrate tests of plugins
([#1612](https://github.com/haskell/haskell-language-server/pull/1612)) by @berberman
- Allow for customizable Haskell views of Property types
([#1608](https://github.com/haskell/haskell-language-server/pull/1608)) by @isovector
- Extract hls-test-utils
([#1606](https://github.com/haskell/haskell-language-server/pull/1606)) by @berberman
- Add test data files to extra-source-files
([#1605](https://github.com/haskell/haskell-language-server/pull/1605)) by @jneira
- Extract stylish-haskell plugin into a standalone package
([#1604](https://github.com/haskell/haskell-language-server/pull/1604)) by @berberman
- Eval plugin: evaluate expressions as statements
([#1603](https://github.com/haskell/haskell-language-server/pull/1603)) by @berberman
- Bump haddock-library to 1.10.0
([#1598](https://github.com/haskell/haskell-language-server/pull/1598)) by @berberman
- Relax ghcides upper bound on base16-bytestring
([#1595](https://github.com/haskell/haskell-language-server/pull/1595)) by @maralorn
- Use CiInterface/SkInterface for typeclass symbols
([#1592](https://github.com/haskell/haskell-language-server/pull/1592)) by @fwcd
- Avoid duplicating known targets and import paths
([#1590](https://github.com/haskell/haskell-language-server/pull/1590)) by @pepeiborra
- Add ability for plugins to handle file change notifications
([#1588](https://github.com/haskell/haskell-language-server/pull/1588)) by @pepeiborra
- Ensure eval plugin Print class doesn't rely on Prelude being in scope
([#1587](https://github.com/haskell/haskell-language-server/pull/1587)) by @akrmn
- Give a canonical ordering for destructing terms in Wingman
([#1586](https://github.com/haskell/haskell-language-server/pull/1586)) by @isovector
- Try a homomorphic destruct before a standard destruct
([#1582](https://github.com/haskell/haskell-language-server/pull/1582)) by @isovector
- Update homepage and other urls for ghcide
([#1580](https://github.com/haskell/haskell-language-server/pull/1580)) by @felixonmars
- Regularize custom config of plugins
([#1576](https://github.com/haskell/haskell-language-server/pull/1576)) by @berberman
- Cleanup the TacticProviders interface
([#1572](https://github.com/haskell/haskell-language-server/pull/1572)) by @isovector
- Add custom code action kinds for import related code actions
([#1570](https://github.com/haskell/haskell-language-server/pull/1570)) by @berberman
- bump retrie plugin version
([#1569](https://github.com/haskell/haskell-language-server/pull/1569)) by @pepeiborra
- Use ConLikes instead of DataCons
([#1568](https://github.com/haskell/haskell-language-server/pull/1568)) by @isovector
- Remove max number of problems config option
([#1567](https://github.com/haskell/haskell-language-server/pull/1567)) by @jneira
- Prepare ghcide 1.1.0
([#1566](https://github.com/haskell/haskell-language-server/pull/1566)) by @pepeiborra
- Use string literals to synthesize the empty string
([#1564](https://github.com/haskell/haskell-language-server/pull/1564)) by @isovector
- Add wingman branding to code actions
([#1555](https://github.com/haskell/haskell-language-server/pull/1555)) by @isovector
- Use TextEdit to insert new imports
([#1554](https://github.com/haskell/haskell-language-server/pull/1554)) by @berberman
- Introduce strict versions of modifyVar to improve contention
([#1553](https://github.com/haskell/haskell-language-server/pull/1553)) by @pepeiborra
- Improve how wingman uses evidence
([#1549](https://github.com/haskell/haskell-language-server/pull/1549)) by @isovector
- Review early cutoff fingerprints
([#1547](https://github.com/haskell/haskell-language-server/pull/1547)) by @pepeiborra
- Improve thread contention around diagnostics
([#1546](https://github.com/haskell/haskell-language-server/pull/1546)) by @pepeiborra
- Be much more intelligent about splitting matches
([#1543](https://github.com/haskell/haskell-language-server/pull/1543)) by @isovector
- Update nixpkgs to ghc 8.10.4
([#1538](https://github.com/haskell/haskell-language-server/pull/1538)) by @berberman
- Log a warning for every diagnostic received when doDiagnostics=False
([#1537](https://github.com/haskell/haskell-language-server/pull/1537)) by @pepeiborra
- Fix missing parens of auto extending imports
([#1526](https://github.com/haskell/haskell-language-server/pull/1526)) by @berberman
- Change Wingman module structure, address -Wall
([#1519](https://github.com/haskell/haskell-language-server/pull/1519)) by @isovector
- Pull Wingman's method hypotheses directly from in-scope dicts
([#1517](https://github.com/haskell/haskell-language-server/pull/1517)) by @isovector
- Avoid redundant work in diagnostics pass
([#1514](https://github.com/haskell/haskell-language-server/pull/1514)) by @pepeiborra
- Add an option to control progress reporting
([#1513](https://github.com/haskell/haskell-language-server/pull/1513)) by @pepeiborra
- Package ghcide code actions
([#1512](https://github.com/haskell/haskell-language-server/pull/1512)) by @berberman
- Demote implicit cradle warn to logging
([#1511](https://github.com/haskell/haskell-language-server/pull/1511)) by @jneira
- Set all plugin flags to manual
([#1510](https://github.com/haskell/haskell-language-server/pull/1510)) by @jneira
- Avoid always rerunning GetModificationTime for interface files too
([#1506](https://github.com/haskell/haskell-language-server/pull/1506)) by @pepeiborra
- Let Wingman's apply tactic run endomorphisms
([#1505](https://github.com/haskell/haskell-language-server/pull/1505)) by @isovector
- Make Wingman produce user-facing error messages
([#1502](https://github.com/haskell/haskell-language-server/pull/1502)) by @isovector
- Disable HLS benchmarks
([#1501](https://github.com/haskell/haskell-language-server/pull/1501)) by @wz1000
- Add kind and preferred flag for all Wingman code actions
([#1499](https://github.com/haskell/haskell-language-server/pull/1499)) by @isovector
- Organize Wingman tests
([#1498](https://github.com/haskell/haskell-language-server/pull/1498)) by @isovector
- Register IDE configuration when called via the command line
([#1495](https://github.com/haskell/haskell-language-server/pull/1495)) by @wz1000
- Haddock upper bound
([#1492](https://github.com/haskell/haskell-language-server/pull/1492)) by @jneira
- Make type lenses plugin configurable
([#1491](https://github.com/haskell/haskell-language-server/pull/1491)) by @berberman
- Context-aware ExactPrint grafting for HsExpr
([#1489](https://github.com/haskell/haskell-language-server/pull/1489)) by @isovector
- Drive GetModificationTime using watched file events
([#1487](https://github.com/haskell/haskell-language-server/pull/1487)) by @pepeiborra
- Faster ModSummary fingerprints
([#1485](https://github.com/haskell/haskell-language-server/pull/1485)) by @pepeiborra
- Revert all changes to hie-compat since 11b5c2e
([#1484](https://github.com/haskell/haskell-language-server/pull/1484)) by @wz1000
- Fix non-determinism in boot-def test
([#1483](https://github.com/haskell/haskell-language-server/pull/1483)) by @wz1000
- Hackage needs autogen-modules
([#1481](https://github.com/haskell/haskell-language-server/pull/1481)) by @jneira
- Ignore ci for some subdirectories and files
([#1480](https://github.com/haskell/haskell-language-server/pull/1480)) by @jneira
- Split plugin tests into two cabal projects
([#1479](https://github.com/haskell/haskell-language-server/pull/1479)) by @wz1000
- Less aggressive refine tactic
([#1475](https://github.com/haskell/haskell-language-server/pull/1475)) by @isovector
- Enable hls-tactics-plugin tests in CI
([#1474](https://github.com/haskell/haskell-language-server/pull/1474)) by @isovector
- Generate a more robust top-level binding Provenance
([#1473](https://github.com/haskell/haskell-language-server/pull/1473)) by @isovector
- Add new variables to the extract when doing intros
([#1472](https://github.com/haskell/haskell-language-server/pull/1472)) by @isovector
- Bump up hlint plugin version
([#1469](https://github.com/haskell/haskell-language-server/pull/1469)) by @jneira
- Make sure split respects GADT equalities
([#1466](https://github.com/haskell/haskell-language-server/pull/1466)) by @isovector
- Add "Split all function arguments" code action
([#1464](https://github.com/haskell/haskell-language-server/pull/1464)) by @isovector
- Add "Refine hole" code action
([#1463](https://github.com/haskell/haskell-language-server/pull/1463)) by @isovector
- Implement "use constructor" code action
([#1461](https://github.com/haskell/haskell-language-server/pull/1461)) by @isovector
- Remove tactics src-dir from func-test
([#1460](https://github.com/haskell/haskell-language-server/pull/1460)) by @isovector
- Make sure to give the correct DynFlags to the recompilation checker
([#1459](https://github.com/haskell/haskell-language-server/pull/1459)) by @pepeiborra
- Don't use record notation for single-field datacons in tactics
([#1456](https://github.com/haskell/haskell-language-server/pull/1456)) by @isovector
- update IRC channel name in plugin tutorial
([#1455](https://github.com/haskell/haskell-language-server/pull/1455)) by @shapr
- Update readme and cabal for Wingman
([#1454](https://github.com/haskell/haskell-language-server/pull/1454)) by @isovector
- Remove recursion tracking from TacticState
([#1453](https://github.com/haskell/haskell-language-server/pull/1453)) by @isovector
- Use runtime ghc libdir for ghc-exactprint and ghc-8.10
([#1451](https://github.com/haskell/haskell-language-server/pull/1451)) by @jneira
- Simplify tactics state structure
([#1449](https://github.com/haskell/haskell-language-server/pull/1449)) by @isovector
- Extract the qualified name from already imported module
([#1445](https://github.com/haskell/haskell-language-server/pull/1445)) by @berberman
- Correct megaparsec lower bound
([#1441](https://github.com/haskell/haskell-language-server/pull/1441)) by @jneira
- Reformat all files
([#1439](https://github.com/haskell/haskell-language-server/pull/1439)) by @Ailrun
- Customize the unitId used for the fake internal component
([#1435](https://github.com/haskell/haskell-language-server/pull/1435)) by @pepeiborra
- Minor performance optimizations
([#1432](https://github.com/haskell/haskell-language-server/pull/1432)) by @pepeiborra

## 1.0.0

This is the celebratory release of Haskell Language Server 1.0.0!
This release includes a lot of internal changes, bug fixes, leaks plugged, and performance improvements, thanks to all our contributors.
Among others,

- We added the support for GHC 8.10.4, and removed the support for GHC 8.10.1
    Afterward, we will support upmost 3 patch versions for each minor version of GHC, if no special situation happens.
- As by hie-bios >= 0.7.3, we use (`${XDG_CACHE_HOME}`)[https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html]`/hie-bios/...` (or similar depends on OS) as a build directory for Cabal.
- Now Eval plugin uses the parsing mechanism of GHC and becomes more stable.
- We supports a code action to disambiguate the same identifiers from multiple imports.
    ![gif](https://user-images.githubusercontent.com/21860/106387152-bfd16d80-641b-11eb-9394-c553fad5794b.gif)
- We supports a code action to hide shadowed identifiers from import.
    ![gif](https://user-images.githubusercontent.com/26041945/107199055-e05b8200-6a30-11eb-9198-448ab8604cc0.gif)
- HIE DB is now integrated. This means we now support `find-references`, `workspace-symbol`. `find-definition` is also improved in a project with multiple components.
- Brittany and Stylish-haskell plugins now load language extensions from the ghc session.
- Tactic plugin supports eta-reduction and Agda-like split tactic and can create a function with piecewise definitions.
    ![gif](https://user-images.githubusercontent.com/307223/107991243-0af68f00-6f8b-11eb-9ff9-3e9a7363ba5f.gif)

### Pull requests merged for 1.0.0

- Correctly split non-varpats in tactics
([#1427](https://github.com/haskell/haskell-language-server/pull/1427)) by @isovector
- Move tactics tests to be standalone
([#1425](https://github.com/haskell/haskell-language-server/pull/1425)) by @isovector
- Fix the handling of default HLS config again
([#1419](https://github.com/haskell/haskell-language-server/pull/1419)) by @pepeiborra
- Patch pre-commit-hook to work with GHCIDE/hls-plugin-api codes
([#1418](https://github.com/haskell/haskell-language-server/pull/1418)) by @Ailrun
- Refactor pragmas plugin
([#1417](https://github.com/haskell/haskell-language-server/pull/1417)) by @berberman
- Add a test for #1376
([#1414](https://github.com/haskell/haskell-language-server/pull/1414)) by @pepeiborra
- Reenable HLS example benchmark
([#1412](https://github.com/haskell/haskell-language-server/pull/1412)) by @pepeiborra
- Fix pre-commit-hook
([#1411](https://github.com/haskell/haskell-language-server/pull/1411)) by @Ailrun
- Plugin Config: Add maxCompletions to JSON instance
([#1407](https://github.com/haskell/haskell-language-server/pull/1407)) by @andys8
- Remove custom 'trim' implementation
([#1406](https://github.com/haskell/haskell-language-server/pull/1406)) by @fendor
- Update to hie-bios 0.7.4
([#1405](https://github.com/haskell/haskell-language-server/pull/1405)) by @fendor
- Prepare 1.0.0 release
([#1402](https://github.com/haskell/haskell-language-server/pull/1402)) by @Ailrun
- Fix install script after hlint fixes
([#1400](https://github.com/haskell/haskell-language-server/pull/1400)) by @jhrcek
- Use last with-utf8 to fix #1372
([#1399](https://github.com/haskell/haskell-language-server/pull/1399)) by @jneira
- Implement Tactic Featuresets
([#1398](https://github.com/haskell/haskell-language-server/pull/1398)) by @isovector
- Update hie-bios
([#1397](https://github.com/haskell/haskell-language-server/pull/1397)) by @Ailrun
- Bump plugins versions
([#1392](https://github.com/haskell/haskell-language-server/pull/1392)) by @pepeiborra
- Split main Tactics module
([#1391](https://github.com/haskell/haskell-language-server/pull/1391)) by @isovector
- Prepare ghcide release 0.7.5
([#1389](https://github.com/haskell/haskell-language-server/pull/1389)) by @pepeiborra
- Disable HLS benchmark example
([#1388](https://github.com/haskell/haskell-language-server/pull/1388)) by @pepeiborra
- Fix GenChangelog script format
([#1387](https://github.com/haskell/haskell-language-server/pull/1387)) by @Ailrun
- Tone down some logInfos to logDebug
([#1385](https://github.com/haskell/haskell-language-server/pull/1385)) by @pepeiborra
- Add a pre commit hook for code formatting
([#1384](https://github.com/haskell/haskell-language-server/pull/1384)) by @Ailrun
- remove unsafePerformIO
([#1383](https://github.com/haskell/haskell-language-server/pull/1383)) by @pepeiborra
- Use object code for TH+UnboxedTuples/Sums
([#1382](https://github.com/haskell/haskell-language-server/pull/1382)) by @wz1000
- Update stack resolvers 8.10.3/8.10.4
([#1380](https://github.com/haskell/haskell-language-server/pull/1380)) by @jneira
- Agda-style case splitting for tactics
([#1379](https://github.com/haskell/haskell-language-server/pull/1379)) by @isovector
- Configuration for initial ghc lib dir
([#1378](https://github.com/haskell/haskell-language-server/pull/1378)) by @pepeiborra
- Use lsp-1.1.1
([#1377](https://github.com/haskell/haskell-language-server/pull/1377)) by @wz1000
- use implicit-hie cradle from setInitialDynFlags
([#1375](https://github.com/haskell/haskell-language-server/pull/1375)) by @wz1000
- Add test for multi-component goto def and make runLanguageServer responsible for hiedb
([#1373](https://github.com/haskell/haskell-language-server/pull/1373)) by @wz1000
- Show window message when auto extending import lists
([#1371](https://github.com/haskell/haskell-language-server/pull/1371)) by @berberman
- Another extension that Brittany cannot parse
([#1369](https://github.com/haskell/haskell-language-server/pull/1369)) by @pepeiborra
- Clean dependency data structures and speed up GetDependencies
([#1368](https://github.com/haskell/haskell-language-server/pull/1368)) by @pepeiborra
- Catch GHC errors in listing module names
([#1367](https://github.com/haskell/haskell-language-server/pull/1367)) by @berberman
- Lose the ghc-lib flag
([#1366](https://github.com/haskell/haskell-language-server/pull/1366)) by @pepeiborra
- Make StylishHaskell plugin recognize extensions from DynFlags
([#1364](https://github.com/haskell/haskell-language-server/pull/1364)) by @Ailrun
- Pass language extensions to Brittany
([#1362](https://github.com/haskell/haskell-language-server/pull/1362)) by @pepeiborra
- Sanitize the setup of the default Ide.Config
([#1361](https://github.com/haskell/haskell-language-server/pull/1361)) by @pepeiborra
- Fix completion snippets on DuplicateRecordFields
([#1360](https://github.com/haskell/haskell-language-server/pull/1360)) by @berberman
- Index files on first open
([#1358](https://github.com/haskell/haskell-language-server/pull/1358)) by @wz1000
- Make find-definition work better with multi-components
([#1357](https://github.com/haskell/haskell-language-server/pull/1357)) by @wz1000
- Construct record datacons in tactics
([#1356](https://github.com/haskell/haskell-language-server/pull/1356)) by @isovector
- Don't insert parentheses for top-level tactics holes
([#1352](https://github.com/haskell/haskell-language-server/pull/1352)) by @isovector
- Simplify extracts after running tactics
([#1351](https://github.com/haskell/haskell-language-server/pull/1351)) by @isovector
- Fix code actions regression
([#1349](https://github.com/haskell/haskell-language-server/pull/1349)) by @pepeiborra
- Refactor the hypothesis type in hls-tactics-plugin
([#1347](https://github.com/haskell/haskell-language-server/pull/1347)) by @isovector
- Fix the Eval plugin sporadic exceptions
([#1345](https://github.com/haskell/haskell-language-server/pull/1345)) by @pepeiborra
- Eval Plugin: Proper handling of flags in `:set`
([#1343](https://github.com/haskell/haskell-language-server/pull/1343)) by @konn
- Cancel earlier queued benchmarks
([#1339](https://github.com/haskell/haskell-language-server/pull/1339)) by @pepeiborra
- Default main for ghcide
([#1338](https://github.com/haskell/haskell-language-server/pull/1338)) by @pepeiborra
- Fix duplication of code actions for adding NamedFieldPuns
([#1334](https://github.com/haskell/haskell-language-server/pull/1334)) by @berberman
- Bump explicit-imports plugin
([#1333](https://github.com/haskell/haskell-language-server/pull/1333)) by @pepeiborra
- Add support for ghc-8.10.4 and drop it for ghc-8.10.1
([#1331](https://github.com/haskell/haskell-language-server/pull/1331)) by @jneira
- Prepare ghcide v0.7.4 release
([#1328](https://github.com/haskell/haskell-language-server/pull/1328)) by @pepeiborra
- Add a new benchmark example to characterise multi-component performance
([#1326](https://github.com/haskell/haskell-language-server/pull/1326)) by @pepeiborra
- [shake-bench] extract project dependencies action (for the HEAD binary)
([#1325](https://github.com/haskell/haskell-language-server/pull/1325)) by @pepeiborra
- [shake-bench] collect eventlogs
([#1324](https://github.com/haskell/haskell-language-server/pull/1324)) by @pepeiborra
- [benchmark] add warmups
([#1323](https://github.com/haskell/haskell-language-server/pull/1323)) by @pepeiborra
- Add code action for hiding shadowed identifiers from imports
([#1322](https://github.com/haskell/haskell-language-server/pull/1322)) by @berberman
- Parallelize benchmark CI
([#1320](https://github.com/haskell/haskell-language-server/pull/1320)) by @pepeiborra
- Fix space leak on cradle reloads
([#1316](https://github.com/haskell/haskell-language-server/pull/1316)) by @pepeiborra
- [benchmarks] speed up CI
([#1315](https://github.com/haskell/haskell-language-server/pull/1315)) by @pepeiborra
- [benchmark] check digests for input files only
([#1314](https://github.com/haskell/haskell-language-server/pull/1314)) by @pepeiborra
- Add link to hackage package to readme
([#1313](https://github.com/haskell/haskell-language-server/pull/1313)) by @expipiplus1
- Splice Plugin: preparatory version bump for the next (not 0.9.0) HLS release
([#1312](https://github.com/haskell/haskell-language-server/pull/1312)) by @konn
- hls-splice-plugin-0.3.0.0-prepare
([#1311](https://github.com/haskell/haskell-language-server/pull/1311)) by @konn
- Trigger extending import only when the item is not in scope
([#1309](https://github.com/haskell/haskell-language-server/pull/1309)) by @berberman
- Bum up hls-eval-plugin to 0.2
([#1305](https://github.com/haskell/haskell-language-server/pull/1305)) by @jneira
- Don't extend import list with child if the parent has been imported as (..)
([#1302](https://github.com/haskell/haskell-language-server/pull/1302)) by @berberman
- Prepare hls hlint plugin 0.2.0
([#1296](https://github.com/haskell/haskell-language-server/pull/1296)) by @jneira
- Import disambiguation: Corrects handling of fully-applied and one-sided sectioned operators in qualifying strategy
([#1294](https://github.com/haskell/haskell-language-server/pull/1294)) by @konn
- hls-splice-plugin-0.2.0.0
([#1293](https://github.com/haskell/haskell-language-server/pull/1293)) by @konn
- Bump haddock comments plugin to 0.1.1
([#1292](https://github.com/haskell/haskell-language-server/pull/1292)) by @berberman
- FindImports typo (minor)
([#1291](https://github.com/haskell/haskell-language-server/pull/1291)) by @andys8
- Bump up hls-plugin-api to 0.7.0
([#1290](https://github.com/haskell/haskell-language-server/pull/1290)) by @jneira
- Prepare ghcide v0.7.3 release
([#1289](https://github.com/haskell/haskell-language-server/pull/1289)) by @pepeiborra
- hls-retrie-plugin 0.1.1.0
([#1288](https://github.com/haskell/haskell-language-server/pull/1288)) by @pepeiborra
- Upgrade to lsp-1.0
([#1284](https://github.com/haskell/haskell-language-server/pull/1284)) by @wz1000
- Update IRC Name in README
([#1275](https://github.com/haskell/haskell-language-server/pull/1275)) by @fendor
- Restore code actions order
([#1273](https://github.com/haskell/haskell-language-server/pull/1273)) by @pepeiborra
- Prepare 0.9.0
([#1271](https://github.com/haskell/haskell-language-server/pull/1271)) by @jneira
- Reenable auto extend imports and drop snippets for infix completions
([#1266](https://github.com/haskell/haskell-language-server/pull/1266)) by @pepeiborra
- ghcide: Implements a CodeAction to disambiguate ambiguous symbols
([#1264](https://github.com/haskell/haskell-language-server/pull/1264)) by @konn
- Doctest comment parsing using module annotations in Eval Plugin
([#1232](https://github.com/haskell/haskell-language-server/pull/1232)) by @konn
- Apply some hlint suggestions, silence some others.
([#1227](https://github.com/haskell/haskell-language-server/pull/1227)) by @peterwicksstringfield
- References via `hiedb`
([#704](https://github.com/haskell/haskell-language-server/pull/704)) by @wz1000
- Use default config on missing configuration section
([#459](https://github.com/haskell/haskell-language-server/pull/459)) by @aufarg

## 0.9.0

This release includes lot of refactorings and bug fixes over existing features, hlint and eval plugins among others.
It contains a fix for a bug in ghcide involving stale diagnostics (#1204).

The list of contributors continues to show healthy growth, many thanks to you all!

And remember, we have a new brand logo, courtesy of @Ailrun :slightly_smiling_face:

![haskell-language-server](https://github.com/haskell/haskell-language-server/raw/master/docs/logos/logo-256.png)

### Pull requests merged for 0.9.0

- Do not error out on failed rewrite
([#1269](https://github.com/haskell/haskell-language-server/pull/1269)) by @pepeiborra
- Tighten dependency on apply-refact
([#1268](https://github.com/haskell/haskell-language-server/pull/1268)) by @hololeap
- Add the new logos
([#1267](https://github.com/haskell/haskell-language-server/pull/1267)) by @Ailrun
- Fix a bug in completions
([#1265](https://github.com/haskell/haskell-language-server/pull/1265)) by @pepeiborra
- Produce heap profiles the old fashioned way, from .hp files
([#1261](https://github.com/haskell/haskell-language-server/pull/1261)) by @pepeiborra
- Break down ghcide functionality in HLS plugins
([#1257](https://github.com/haskell/haskell-language-server/pull/1257)) by @pepeiborra
- Enforce max completions over all plugins
([#1256](https://github.com/haskell/haskell-language-server/pull/1256)) by @pepeiborra
- Reorder code actions to put remove redundant imports first
([#1255](https://github.com/haskell/haskell-language-server/pull/1255)) by @pepeiborra
- Update bench.yml to include all the relevant artifacts
([#1254](https://github.com/haskell/haskell-language-server/pull/1254)) by @pepeiborra
- Benchmarks: generate heap profiles
([#1253](https://github.com/haskell/haskell-language-server/pull/1253)) by @pepeiborra
- Add gh workflows badges
([#1251](https://github.com/haskell/haskell-language-server/pull/1251)) by @jneira
- Add dynamic linking common issue
([#1249](https://github.com/haskell/haskell-language-server/pull/1249)) by @jneira
- Add license for hls-tactics-plugin
([#1248](https://github.com/haskell/haskell-language-server/pull/1248)) by @isovector
- Use exact print to extend import lists
([#1246](https://github.com/haskell/haskell-language-server/pull/1246)) by @berberman
- Test apply-refact with TypeApplications
([#1244](https://github.com/haskell/haskell-language-server/pull/1244)) by @jneira
- Add non reversable pragma completion
([#1243](https://github.com/haskell/haskell-language-server/pull/1243)) by @Ailrun
- Delete redundant "category: Development".
([#1241](https://github.com/haskell/haskell-language-server/pull/1241)) by @peterwicksstringfield
- Complete the No- variants of language extensions and Strict extension
([#1238](https://github.com/haskell/haskell-language-server/pull/1238)) by @mrBliss
- Add code actions for disabling a warning in the current file
([#1235](https://github.com/haskell/haskell-language-server/pull/1235)) by @georgefst
- Change packages metadata and rename tactics subfolder
([#1234](https://github.com/haskell/haskell-language-server/pull/1234)) by @jneira
- Fix the bug that generating comments would duplicate existing comments
([#1233](https://github.com/haskell/haskell-language-server/pull/1233)) by @berberman
- Delete global hie.yaml config
([#1230](https://github.com/haskell/haskell-language-server/pull/1230)) by @jneira
- Easy hlint fixes
([#1226](https://github.com/haskell/haskell-language-server/pull/1226)) by @peterwicksstringfield
- Use the runtime ghc libdir for ghc-exactprint
([#1225](https://github.com/haskell/haskell-language-server/pull/1225)) by @jneira
- Add note in README/Tutorial regarding CPP support
([#1224](https://github.com/haskell/haskell-language-server/pull/1224)) by @tittoassini
- Test and fix for issue 1213
([#1223](https://github.com/haskell/haskell-language-server/pull/1223)) by @tittoassini
- Add traces for HLS providers
([#1222](https://github.com/haskell/haskell-language-server/pull/1222)) by @pepeiborra
- Use exact print for suggest missing constraint code actions
([#1221](https://github.com/haskell/haskell-language-server/pull/1221)) by @pepeiborra
- Fix changelog dates
([#1220](https://github.com/haskell/haskell-language-server/pull/1220)) by @pepeiborra
- Ignore .shake folder
([#1219](https://github.com/haskell/haskell-language-server/pull/1219)) by @pepeiborra
- Limit completions to top 40
([#1218](https://github.com/haskell/haskell-language-server/pull/1218)) by @pepeiborra
- Parenthesise type operators when extending import lists
([#1212](https://github.com/haskell/haskell-language-server/pull/1212)) by @mrBliss
- Expose shake options used
([#1209](https://github.com/haskell/haskell-language-server/pull/1209)) by @pepeiborra
- Prepare ghcide release v0.7.1
([#1207](https://github.com/haskell/haskell-language-server/pull/1207)) by @pepeiborra
- Documentation for the Eval Plugin
([#1206](https://github.com/haskell/haskell-language-server/pull/1206)) by @tittoassini
- Stale diagnostics fix
([#1204](https://github.com/haskell/haskell-language-server/pull/1204)) by @pepeiborra
- Extract Development.IDE.GHC.ExactPrint
([#1203](https://github.com/haskell/haskell-language-server/pull/1203)) by @pepeiborra
- Fix bug in Retrie "fold/unfold in local file" commands
([#1202](https://github.com/haskell/haskell-language-server/pull/1202)) by @pepeiborra
- Minor eval plugin fixes
([#1199](https://github.com/haskell/haskell-language-server/pull/1199)) by @tittoassini
- Disable win 8.6.4 job
([#1198](https://github.com/haskell/haskell-language-server/pull/1198)) by @jneira
- Add custom cache layer for session loading
([#1197](https://github.com/haskell/haskell-language-server/pull/1197)) by @fendor
- Use completionSnippetsOn flag
([#1195](https://github.com/haskell/haskell-language-server/pull/1195)) by @takoeight0821
- Remove runs dropped by #1173
([#1194](https://github.com/haskell/haskell-language-server/pull/1194)) by @jneira
- Remove undefined exports suggestions
([#1193](https://github.com/haskell/haskell-language-server/pull/1193)) by @kderme
- Update nixpkgs to ghc 8.10.3
([#1191](https://github.com/haskell/haskell-language-server/pull/1191)) by @pepeiborra
- Do not disable parallel GC
([#1190](https://github.com/haskell/haskell-language-server/pull/1190)) by @pepeiborra
- Switch module outline to useWtihStale
([#1189](https://github.com/haskell/haskell-language-server/pull/1189)) by @pepeiborra
- Fix sticky diagnostics
([#1188](https://github.com/haskell/haskell-language-server/pull/1188)) by @pepeiborra
- Fix class plugin cabal
([#1186](https://github.com/haskell/haskell-language-server/pull/1186)) by @Ailrun
- Update package description of haddock comments plugin
([#1185](https://github.com/haskell/haskell-language-server/pull/1185)) by @berberman
- Installation from Hackage - add README section
([#1183](https://github.com/haskell/haskell-language-server/pull/1183)) by @pepeiborra
- Preparation for Uploading Splice Plugin to Hackage
([#1182](https://github.com/haskell/haskell-language-server/pull/1182)) by @konn
- Preparation for uploading `hls-exactprint-utils`
([#1181](https://github.com/haskell/haskell-language-server/pull/1181)) by @konn
- Complete hls-hlint-plugin package metadata
([#1180](https://github.com/haskell/haskell-language-server/pull/1180)) by @jneira
- Benchmark improvements
([#1178](https://github.com/haskell/haskell-language-server/pull/1178)) by @pepeiborra
- Make adding missing constraint work in presence of 'forall' (fixes #1164)
([#1177](https://github.com/haskell/haskell-language-server/pull/1177)) by @jhrcek
- Prepare for Hackage
([#1176](https://github.com/haskell/haskell-language-server/pull/1176)) by @pepeiborra
- Test only last ghc minor version and fix windows cache
([#1173](https://github.com/haskell/haskell-language-server/pull/1173)) by @jneira
- Fix toMethodName bug of the Class plugin
([#1170](https://github.com/haskell/haskell-language-server/pull/1170)) by @Ailrun
- Quick fix for #1158
([#1166](https://github.com/haskell/haskell-language-server/pull/1166)) by @Ailrun
- Suggest adding pragmas for parse errors too
([#1165](https://github.com/haskell/haskell-language-server/pull/1165)) by @mrBliss
- Fix wrong component name of splice plugin in hie.yaml
([#1162](https://github.com/haskell/haskell-language-server/pull/1162)) by @berberman
- Revert "Auto cancel redundant workflows (attempt #2)"
([#1156](https://github.com/haskell/haskell-language-server/pull/1156)) by @pepeiborra
- Auto cancel redundant workflows (attempt #2)
([#1154](https://github.com/haskell/haskell-language-server/pull/1154)) by @pepeiborra
- Prepare 0.8.0 (versions)
([#1153](https://github.com/haskell/haskell-language-server/pull/1153)) by @jneira
- Streamline CircleCI jobs
([#1152](https://github.com/haskell/haskell-language-server/pull/1152)) by @pepeiborra
- Mergify: create configuration
([#1151](https://github.com/haskell/haskell-language-server/pull/1151)) by @jneira
- Bump haskell-lsp to 0.23
([#1146](https://github.com/haskell/haskell-language-server/pull/1146)) by @berberman
- Remove no longer needed git submodule update
([#1145](https://github.com/haskell/haskell-language-server/pull/1145)) by @jhrcek
- Enable more tests
([#1143](https://github.com/haskell/haskell-language-server/pull/1143)) by @peterwicksstringfield
- Update links to issues/PRs in ghcide tests.
([#1142](https://github.com/haskell/haskell-language-server/pull/1142)) by @peterwicksstringfield
- Fix #723 (Instance declarations in hs-boot files result in GHC errors)
([#781](https://github.com/haskell/haskell-language-server/pull/781)) by @nitros12
- Also suggest importing methods without parent class
([#766](https://github.com/haskell/haskell-language-server/pull/766)) by @mrBliss
- Delete unused utilities for controlling logging.
([#764](https://github.com/haskell/haskell-language-server/pull/764)) by @peterwicksstringfield
- Delete unused testdata
([#763](https://github.com/haskell/haskell-language-server/pull/763)) by @peterwicksstringfield
- Fix suggestAddTypeAnnotation regex
([#760](https://github.com/haskell/haskell-language-server/pull/760)) by @kderme
- Splice Plugin: expands TH splices and QuasiQuotes
([#759](https://github.com/haskell/haskell-language-server/pull/759)) by @konn
- Haddock comments plugin
([#673](https://github.com/haskell/haskell-language-server/pull/673)) by @berberman
- Leverage last apply-refact improvements in hlint plugin (include getParsedModuleWithComments in ghcide)
([#635](https://github.com/haskell/haskell-language-server/pull/635)) by @jneira

## 0.8.0

- This version adds support for ghc-8.10.3
- `hls-plugin-api` has been bumped to 0.6.0.0 and `ghcide` has been bumped from 0.6.0.1 to 0.7.0.0.
- It has a new brand plugin: hls-class-plugin, which helps to write class instances

![gif](https://user-images.githubusercontent.com/12473268/103059293-af071f80-4572-11eb-963a-7e76b45f28b9.gif)

- The eval plugin has been revamped, adding these new features:
  - Tests in both plain comments and Haddock comments
  - For Haddock comments: shows differences between latest and previous result
  - Setup section, executed before every test
  - Execution of a section/group of tests at the time
  - Property testing
  - Setup of GHC extensions
- A new tactic to generate automatically `Arbitrary` instances has been added to tactic plugin
- There had been lot of internal changes:
  - ghcide lives now directly in this repository
  - the test suite has been cleaned and improved (continuing the work done in 0.7.0)

Thanks to all contributors and happy new year!

### Pull requests merged for 0.8.0

- Ci fixes
([#783](https://github.com/haskell/haskell-language-server/pull/783)) by @pepeiborra
- Fix extend imports regression
([#769](https://github.com/haskell/haskell-language-server/pull/769)) by @pepeiborra
- Cleanup format testfiles
([#765](https://github.com/haskell/haskell-language-server/pull/765)) by @peterwicksstringfield
- Retry a failed cradle if the cradle descriptor changes
([#762](https://github.com/haskell/haskell-language-server/pull/762)) by @pepeiborra
- Perform memory measurement on SIGUSR1
([#761](https://github.com/haskell/haskell-language-server/pull/761)) by @pepeiborra
- Add ghc-8.10.3 support after merging ghcide repo
([#721](https://github.com/haskell/haskell-language-server/pull/721)) by @jneira
- Merge ghcide repository (replacing the submodule)
([#702](https://github.com/haskell/haskell-language-server/pull/702)) by @pepeiborra
- Invert the dependency between hls-plugin-api and ghcide
([#701](https://github.com/haskell/haskell-language-server/pull/701)) by @pepeiborra
- Move eval plugin to hls-eval-plugin
([#700](https://github.com/haskell/haskell-language-server/pull/700)) by @tittoassini
- Fix and enable progress message tests.
([#698](https://github.com/haskell/haskell-language-server/pull/698)) by @peterwicksstringfield
- Add a known tactic for writing arbitrary instances
([#695](https://github.com/haskell/haskell-language-server/pull/695)) by @isovector
- Introduce generic config for plugins
([#691](https://github.com/haskell/haskell-language-server/pull/691)) by @alanz
- Enable get type definition tests
([#690](https://github.com/haskell/haskell-language-server/pull/690)) by @peterwicksstringfield
- Fix ghc version for windows 8.10.2.2 in github build workflow
([#688](https://github.com/haskell/haskell-language-server/pull/688)) by @jneira
- Add plugins conditionally at compile time
([#687](https://github.com/haskell/haskell-language-server/pull/687)) by @jneira
- Implement basic Class plugin
([#661](https://github.com/haskell/haskell-language-server/pull/661)) by @Ailrun
- Extended Eval Plugin
([#438](https://github.com/haskell/haskell-language-server/pull/438)) by @tittoassini

## 0.7.1

- This is a minor bug fix release:
  - It fixes an issue that removed accidentally desugarer warnings (#676).
  - It disables auto extend import lists in completions, see #679.

### Pull requests merged for 0.7.1

- Disable auto extend import lists in completions. It fixes #679.
([#685](https://github.com/haskell/haskell-language-server/pull/685)) by @pepeiborra
- Restore kick (#676). It fixes #676.
([#677](https://github.com/haskell/haskell-language-server/pull/677)) by @wz1000
- README: Remove instructions to execute data target
([#675](https://github.com/haskell/haskell-language-server/pull/675)) by @andys8
- Add hlint tests over cpp, extensions and ignore hints
([#674](https://github.com/haskell/haskell-language-server/pull/674)) by @jneira

## 0.7.0

- This version contains mainly refactors and updates of upstream packages
- It bumps up some formatter versions:
  - ormolu is 0.1.4.1
  - fourmolu is 0.3.0.0
  - brittany is 0.13.1.0
- It uses last implicit-hie-cradle-0.3.0.2, with some [bug](https://github.com/Avi-D-coder/implicit-hie/issues/29) [fixes](https://github.com/Avi-D-coder/implicit-hie/issues/30)
- It uses last ghcide-0.6.0.1 with [improvements and bug fixes](https://github.com/haskell/ghcide/blob/master/CHANGELOG.md#060-2020-12-06):
  - Do not enable every "unnecessary" warning by default
  - Improvements over completions:
    - record fields
    - identifiers not in explicit import lists
    - extend explicit import list automatically

Thanks to all haskell-language-server, ghcide and other upstream packages contributors (the list continue growing healthy) for make this release possible.

### Pull requests merged for 0.7.0

- Miscellanous fixes: correct tactic plugin package metadata and cabal.hie.yaml/stack.hie.yaml
([#672](https://github.com/haskell/haskell-language-server/pull/672)) by @berberman
- Remove unnecessary pluginId setting and user Better Map functions in tactics plugin
([#669](https://github.com/haskell/haskell-language-server/pull/669)) by @jhrcek
- Do not suggest explicitly disabled pragmas
([#666](https://github.com/haskell/haskell-language-server/pull/666)) by @berberman
- fixed hie.yaml.stack
([#664](https://github.com/haskell/haskell-language-server/pull/664)) by @tittoassini
- Add pragmas completions
([#662](https://github.com/haskell/haskell-language-server/pull/662)) by @gdevanla
- Enable code completion tests
([#657](https://github.com/haskell/haskell-language-server/pull/657)) by @peterwicksstringfield
- Enable highlight unittests
([#656](https://github.com/haskell/haskell-language-server/pull/656)) by @peterwicksstringfield
- Fix document symbols unit tests.
([#655](https://github.com/haskell/haskell-language-server/pull/655)) by @peterwicksstringfield
- Delete duplicate cabal clause for applyrefact2
([#654](https://github.com/haskell/haskell-language-server/pull/654)) by @peterwicksstringfield
- Add extra-source-files for split plugins
([#650](https://github.com/haskell/haskell-language-server/pull/650)) by @berberman
- [nix-shell] Actually use gitignore
([#649](https://github.com/haskell/haskell-language-server/pull/649)) by @pepeiborra
- idempotent command and code cleanup
([#648](https://github.com/haskell/haskell-language-server/pull/648)) by @tittoassini
- Split the Imports and Retrie plugins
([#647](https://github.com/haskell/haskell-language-server/pull/647)) by @pepeiborra
- Simplify and Bump implicit-hie version constraints
([#645](https://github.com/haskell/haskell-language-server/pull/645)) by @Avi-D-coder
- Fix and enable disabled code action unit tests, fix fallback handler
([#643](https://github.com/haskell/haskell-language-server/pull/643)) by @peterwicksstringfield
- Add Ghcide hie.yaml instruction for Stack users
([#641](https://github.com/haskell/haskell-language-server/pull/641)) by @Sir4ur0n
- Upgrade the Nix build system
([#639](https://github.com/haskell/haskell-language-server/pull/639)) by @pepeiborra
- No longer needed to build once for Stack
([#637](https://github.com/haskell/haskell-language-server/pull/637)) by @Sir4ur0n
- Preserve the last empty comment line after eval plugin
([#631](https://github.com/haskell/haskell-language-server/pull/631)) by @expipiplus1
- Update fourmolu to 0.3.0.0
([#624](https://github.com/haskell/haskell-language-server/pull/624)) by @gwils
- Add hspec-discover to build-tool-depends in tactics plugin
([#623](https://github.com/haskell/haskell-language-server/pull/623)) by @gwils
- Add build to ghc-8.10.2 and windows
([#619](https://github.com/haskell/haskell-language-server/pull/619)) by @jneira
- Module Name Plugin: Treat modules starting with lowercase as Main module
([#616](https://github.com/haskell/haskell-language-server/pull/616)) by @konn
- Bump ormolu to 0.1.4.1
([#614](https://github.com/haskell/haskell-language-server/pull/614)) by @AlistairB
- Fix fourmolu plugin inconsistent formatting
([#599](https://github.com/haskell/haskell-language-server/pull/599)) by @zweimach
- Hlint: bring over idea2Message for formatting
([#598](https://github.com/haskell/haskell-language-server/pull/598)) by @alanz
- Makes dictionary argument exclusion logic in Tactic plugin more robust
([#508](https://github.com/haskell/haskell-language-server/pull/508)) by @konn

## 0.6.0

0.6.0 includes two brand new plugins!

- [Hlint Plugin](https://github.com/haskell/haskell-language-server/pull/166): it integrates hlint diagnostics and lets you apply suggestions to fix them.

![hls-hlint-demo](https://user-images.githubusercontent.com/54035/98731058-6ff38500-239d-11eb-8176-e4f69ef76fc2.gif)

- [Module Name Plugin](https://github.com/haskell/haskell-language-server/pull/480): it makes easier create new modules and modify them, suggesting the appropiate module name as a code lens.

![module-name-demo](https://user-images.githubusercontent.com/54035/98731198-a7623180-239d-11eb-8af0-73bd32b9b0b2.gif)

This release also includes many improvements and bug fixes for the tactic plugin (see pull requests authored by @isovector for more details).

We have updated two essential tools used by the ide:

- `implicit-hie`: [to fix a bug](https://github.com/haskell/haskell-language-server/issues/498) present when loading cabal based projects with executables containing `other-modules`

- `ghcide`: the ide uses [the just released version 0.5](https://github.com/haskell/ghcide/blob/master/CHANGELOG.md#050-2020-10-08) with many bug fixes and improvements, including:
  - code action to remove *all* redundant imports
  - improved support for Template Haskell
  - emit desugarer warnings

### Pull requests merged for 0.6.0

- Fix tasty rerun
([#570](https://github.com/haskell/haskell-language-server/pull/570)) by @jneira
- Bump up ghcide submodule to version 0.5.0
([#568](https://github.com/haskell/haskell-language-server/pull/568)) by @jneira
- Refactor tactics to track hypothesis provenance
([#557](https://github.com/haskell/haskell-language-server/pull/557)) by @isovector
- Use bash shell to allow its idioms
([#552](https://github.com/haskell/haskell-language-server/pull/552)) by @jneira
- Ignore flakey tactics test
([#546](https://github.com/haskell/haskell-language-server/pull/546)) by @isovector
- Better scoring metric for deriving safeHead
([#545](https://github.com/haskell/haskell-language-server/pull/545)) by @isovector
- Discover skolems in the hypothesis, not just goal
([#542](https://github.com/haskell/haskell-language-server/pull/542)) by @isovector
- [retrie] Fix code action title
([#538](https://github.com/haskell/haskell-language-server/pull/538)) by @pepeiborra
- Tactics support for using given constraints
([#534](https://github.com/haskell/haskell-language-server/pull/534)) by @isovector
- Add missing tactic subpackage in default stack.yaml
([#529](https://github.com/haskell/haskell-language-server/pull/529)) by @jneira
- Use implicit-hie-0.1.2.0
([#528](https://github.com/haskell/haskell-language-server/pull/528)) by @jneira
- Wait for diagnostics in tactics tests
([#525](https://github.com/haskell/haskell-language-server/pull/525)) by @isovector
- Fix a bug in tactics preventing split of split
([#520](https://github.com/haskell/haskell-language-server/pull/520)) by @isovector
- Use infix notation for destructing and splitting infix data cons
([#519](https://github.com/haskell/haskell-language-server/pull/519)) by @isovector
- Retry the build three times
([#518](https://github.com/haskell/haskell-language-server/pull/518)) by @jneira
- Separate tactics into its own package
([#516](https://github.com/haskell/haskell-language-server/pull/516)) by @isovector
- Add a Troubleshooting section to the README
([#507](https://github.com/haskell/haskell-language-server/pull/507)) by @michaelpj
- Add GitHub Actions CI for testing
([#504](https://github.com/haskell/haskell-language-server/pull/504)) by @bubba
- Fix stack build for ghc-8.8.3 failing on some machines
([#503](https://github.com/haskell/haskell-language-server/pull/503)) by @luntain
- Expand explanation of how to configure HLS
([#497](https://github.com/haskell/haskell-language-server/pull/497)) by @michaelpj
- Module Name Plugin
([#480](https://github.com/haskell/haskell-language-server/pull/480)) by @tittoassini
- Allow hole filling to deal with recursion
([#472](https://github.com/haskell/haskell-language-server/pull/472)) by @isovector
- Restrict editor config to Haskell file, to avoid affecting Makefiles or other tab-based formats
([#442](https://github.com/haskell/haskell-language-server/pull/442)) by @tittoassini
- Hlint plugin using ghc-lib
([#166](https://github.com/haskell/haskell-language-server/pull/166)) by @jneira

## 0.5.1

0.5.1 is a minor bug fix release, mainly fixing an issue with the eval plugin
as well as upgrading the ormolu and stylish-haskell dependencies.

### Pull requests merged for 0.5.1

- Minimal fix for eval regression
([#488](https://github.com/haskell/haskell-language-server/pull/488)) by @pepeiborra
- Bump stylish-haskell to 0.12.2.0
([#482](https://github.com/haskell/haskell-language-server/pull/482)) by @maksbotan
- Improve the emacs instructions a little
([#479](https://github.com/haskell/haskell-language-server/pull/479)) by @michaelpj
- Update README: HLS is no longer in *very* early stage
([#475](https://github.com/haskell/haskell-language-server/pull/475)) by @Anrock
- Tactic plugin: Excludes Dictionary arguments in GADTs in Destruct Tactic
([#474](https://github.com/haskell/haskell-language-server/pull/474)) by @konn
- Update doom emacs install instructions in README
([#470](https://github.com/haskell/haskell-language-server/pull/470)) by @iyefrat
- Add ghc-8.10.2 to circleci
([#464](https://github.com/haskell/haskell-language-server/pull/464)) by @jneira
- Bump ormolu to 0.1.3.0
([#422](https://github.com/haskell/haskell-language-server/pull/422)) by @AlistairB

## 0.5.0

0.5.0 comes with a new tactics plugin which provides case splitting, homomorphic case splitting, and lambda introduction:

![Case splitting](https://user-images.githubusercontent.com/307223/92657198-3d4be400-f2a9-11ea-8ad3-f541c8eea891.gif)

It can even attempt to fully fill a hole!

![Attempt to fill in hole code action](https://user-images.githubusercontent.com/307223/94743611-82a18580-032c-11eb-9f13-8f46bc45f928.gif)

The imports lens plugin also learnt a new code action to make all imports explicit:

![Explicit imports code action](https://user-images.githubusercontent.com/2488460/94994815-1a53dd80-0592-11eb-8a12-ec704ae92385.gif)

There's also plenty of bug fixes, improvements and updates to the underlying tools, including Fourmolu, implicit-hie-cradle and ghcide. [Some of the improvements from ghcide](https://github.com/haskell/ghcide/releases/tag/v0.4.0) include:

- The entire project is typechecked on load
- Reverse dependencies of a module are typechecked upon saving
- Code completion includes local terms
- Import code actions now also suggest open imports
- Documentation on hover shows for symbols defined in the same module

If you're eager to try all this out, haskell-language-server is now also installable via [ghcup](https://www.haskell.org/ghcup/):

```shell
> ghcup install hls
```

### Pull requests merged for 0.5.0

- Update GHC version 8.12 to 9.0 in README
([#460](https://github.com/haskell/haskell-language-server/pull/460)) by @maralorn
- Update Fourmolu to 0.2
([#455](https://github.com/haskell/haskell-language-server/pull/455)) by @georgefst
- Generate .gz tars of all the binaries for macOS and Linux in GitHub Actions
([#454](https://github.com/haskell/haskell-language-server/pull/454)) by @bubba
- install: create hls hardlinks instead of copies except on Windows
([#451](https://github.com/haskell/haskell-language-server/pull/451)) by @juhp
- wrapper: cd to --cwd earlier
([#448](https://github.com/haskell/haskell-language-server/pull/448)) by @ocharles
- Update README.md
([#446](https://github.com/haskell/haskell-language-server/pull/446)) by @moodmosaic
- Upate Emacs setup notes
([#440](https://github.com/haskell/haskell-language-server/pull/440)) by @gdevanla
- Use ghcide master and prepare hls-plugin-api-0.4.1.0
([#439](https://github.com/haskell/haskell-language-server/pull/439)) by @jneira
- Add a code action to make all imports explicit
([#436](https://github.com/haskell/haskell-language-server/pull/436)) by @pepeiborra
- Add docs on how to choose a formatter
([#432](https://github.com/haskell/haskell-language-server/pull/432)) by @googleson78
- Implement 'Attempt to fill hole' code action
([#431](https://github.com/haskell/haskell-language-server/pull/431)) by @TOTBWF
- Clarify that eval is a lens
([#428](https://github.com/haskell/haskell-language-server/pull/428)) by @Anrock
- Use implicit-hie-cradle-0.2.0.1
([#427](https://github.com/haskell/haskell-language-server/pull/427)) by @jneira
- [retrie] Fix uris in workspace edit
([#424](https://github.com/haskell/haskell-language-server/pull/424)) by @pepeiborra
- Separate paragraphs
([#423](https://github.com/haskell/haskell-language-server/pull/423)) by @jneira
- Include .editorconfig in the contributing section
([#420](https://github.com/haskell/haskell-language-server/pull/420)) by @jneira
- Mention the copy of executables wit ghc version
([#419](https://github.com/haskell/haskell-language-server/pull/419)) by @jneira
- Eval plugin: proper multilined results handling and command-name abbreviations
([#413](https://github.com/haskell/haskell-language-server/pull/413)) by @konn
- Retrie - calculate imports in the command handler
([#408](https://github.com/haskell/haskell-language-server/pull/408)) by @pepeiborra
- Progress reporting for Eval plugin
([#398](https://github.com/haskell/haskell-language-server/pull/398)) by @pepeiborra
- bump ghcide submodule
([#396](https://github.com/haskell/haskell-language-server/pull/396)) by @wz1000
- Fix cradles
([#393](https://github.com/haskell/haskell-language-server/pull/393)) by @pepeiborra
- Case splitting and lambda introduction
([#391](https://github.com/haskell/haskell-language-server/pull/391)) by @isovector
- Use stale data in explicit imports lens
([#383](https://github.com/haskell/haskell-language-server/pull/383)) by @pepeiborra
- Create hls-plugin-api and move plugins to exe
([#379](https://github.com/haskell/haskell-language-server/pull/379)) by @jneira
- Rebase on ghcide HEAD
([#378](https://github.com/haskell/haskell-language-server/pull/378)) by @pepeiborra
- README clarify how exactly to use code evaluation
([#377](https://github.com/haskell/haskell-language-server/pull/377)) by @DunetsNM
- Revise README.md
([#374](https://github.com/haskell/haskell-language-server/pull/374)) by @gihyeonsung

## 0.4.0

0.4.0 introduces the import lens plugin, which can convert your import statements into qualified imports, or into an explicit import list:

![Imports code lens](https://imgur.com/pX9kvY4.gif)

The eval plugin has also learnt two new commands, `:type` and `:kind`:

```haskell
{-# LANGUAGE TypeApplications #-}
foo :: Show a => a -> String
foo = show

-- >>> :type foo @Int
-- foo @Int :: Int -> String

-- >>> :type +v foo @Int
-- foo @Int :: Show Int => Int -> String
```

```haskell
-- >>> type N = 1
-- >>> type M = 40
-- >>> :kind N + M + 1
-- N + M + 1 :: Nat

-- >>> type N = 1
-- >>> type M = 40
-- >>> :kind N + M + 1
-- N + M + 1 :: Nat
```

There is now also support for GHC 8.10.2, and a new `haskell-language-server --probe-tools` command to help debug what version of each tool HLS is using.

```shell
$ haskell-language-server --probe-tools
haskell-language-server version: 0.3.0.0 (GHC: 8.10.1) (PATH: /Users/luke/.cabal/store/ghc-8.10.1/hskll-lngg-srvr-0.3.0.0-7c6d48c3/bin/haskell-language-server)
Tool versions found on the $PATH
cabal:  3.2.0.0
stack:  2.3.3
ghc:    8.10.2
```

### Pull requests merged for 0.4.0

- Bring over a [tutorial about how to add hls plugins](https://github.com/pepeiborra/hls-tutorial)
([#372](https://github.com/haskell/haskell-language-server/pull/372)) by @bubba
- Update the ghcide upstream to be in haskell/ghcide
([#370](https://github.com/haskell/haskell-language-server/pull/370)) by @alanz
- Add ISSUE_TEMPLATE for github
([#305](https://github.com/haskell/haskell-language-server/pull/305)) by @fendor
- Add use-package to the list of emacs packages
([#343](https://github.com/haskell/haskell-language-server/pull/343)) by @rgleichman
- Implements `:type [+v/+d]` in Eval Plugin
([#361](https://github.com/haskell/haskell-language-server/pull/361)) by @konn
- Bump bounds of hie-bios to 0.7.0
([#357](https://github.com/haskell/haskell-language-server/pull/357)) by @maralorn
- Fix ImportLens plugin to work with GHC 8.10
([#356](https://github.com/haskell/haskell-language-server/pull/356)) by @Ailrun
- Add single file rewrites and ignore unknown files
([#321](https://github.com/haskell/haskell-language-server/pull/321)) by @pepeiborra
- Do not suggest explicit import lists for qualified imports
([#354](https://github.com/haskell/haskell-language-server/pull/354)) by @expipiplus1
- Explicit imports lens (as seen on Twitter)
([#310](https://github.com/haskell/haskell-language-server/pull/310)) by @pepeiborra
- Adds `:kind` and `:kind!` commands to Eval Plugin
([#345](https://github.com/haskell/haskell-language-server/pull/345)) by @konn
- tech(nix): update niv and remove allowbroken
([#350](https://github.com/haskell/haskell-language-server/pull/350)) by @willbush
- Update VS Code Haskell URL/repo
([#338](https://github.com/haskell/haskell-language-server/pull/338)) by @Sir4ur0n
- doc(hack): Add explanation to hack and test HLS
([#329](https://github.com/haskell/haskell-language-server/pull/329)) by @Sir4ur0n
- Apply the module pragmas for evaluation
([#322](https://github.com/haskell/haskell-language-server/pull/322)) by @pepeiborra
- Copy working stack-8.6.5.yaml to stack.yaml
([#332](https://github.com/haskell/haskell-language-server/pull/332)) by @jneira
- tech(nix): Allow broken as retrie is marked as broken
([#331](https://github.com/haskell/haskell-language-server/pull/331)) by @Sir4ur0n
- feat(git): Add install/hie.yaml to gitignore
([#328](https://github.com/haskell/haskell-language-server/pull/328)) by @Sir4ur0n
- Replace wrong occurrences of "engine" by "server"
([#319](https://github.com/haskell/haskell-language-server/pull/319)) by @tchoutri
- Simplify coc.nvim instructions
([#315](https://github.com/haskell/haskell-language-server/pull/315)) by @oblitum
- Coc config file requires a {} nesting everything
([#317](https://github.com/haskell/haskell-language-server/pull/317)) by @hyiltiz
- Restrict opentelemetry version for stack builds
([#312](https://github.com/haskell/haskell-language-server/pull/312)) by @jneira
- Add support for ghc-8.10.2
([#308](https://github.com/haskell/haskell-language-server/pull/308)) by @jneira
- Return nothing if tool is not on the PATH
([#309](https://github.com/haskell/haskell-language-server/pull/309)) by @fendor
- Probe tools cli
([#306](https://github.com/haskell/haskell-language-server/pull/306)) by @fendor
- Add fourmolu plugin (attempt 2) and add Brittany for ghc-8.10.1
([#264](https://github.com/haskell/haskell-language-server/pull/264)) by @georgefst

## 0.3.0

0.3.0 comes with two new plugins, retrie and fourmolu, provides binaries for
GHC 8.8.4, and comes with a host of bug fixes.

The retrie plugin supports RULES, functions and type synonyms which can be
accessed through contextual code actions.

Fourmolu can be used to format your code by setting the
`haskell.formattingProvider` field in your LSP configuration to

```json
{
  "haskell": {
    "formattingProvider": "fourmolu"
  }
}
```

The Brittany formatter is now also available on GHC 8.10.1.

### Pull requests merged

- Fix haddock parse error in install.hs
([#255](https://github.com/haskell/haskell-language-server/pull/255)) by @georgefst
- Ormolu flags
([#246](https://github.com/haskell/haskell-language-server/pull/246)) by @pepeiborra
- Ormolu fix
([#257](https://github.com/haskell/haskell-language-server/pull/257)) by @sureyeaah
- Remove redundant CircleCI steps
([#259](https://github.com/haskell/haskell-language-server/pull/259)) by @bubba
- Slow down Tasty by limiting it to -j1
([#261](https://github.com/haskell/haskell-language-server/pull/261)) by @bubba
- Remove hspec-expectations
([#260](https://github.com/haskell/haskell-language-server/pull/260)) by @bubba
- Remove a redundant caching step
([#262](https://github.com/haskell/haskell-language-server/pull/262)) by @Ailrun
- add hie.yaml to coc configuration
([#267](https://github.com/haskell/haskell-language-server/pull/267)) by @sureyeaah
- Initial Retrie plugin
([#266](https://github.com/haskell/haskell-language-server/pull/266)) by @pepeiborra
- Add exe extension to win executables
([#284](https://github.com/haskell/haskell-language-server/pull/284)) by @jneira
- Use wz1000/hls-3 ghcide branch
([#275](https://github.com/haskell/haskell-language-server/pull/275)) by @alanz
- Fix rename capability being declared
([#285](https://github.com/haskell/haskell-language-server/pull/285)) by @bubba
- Add CI job for 8.8.4
([#287](https://github.com/haskell/haskell-language-server/pull/287)) by @bubba
- Make the AGPL flag manual in cabal
([#250](https://github.com/haskell/haskell-language-server/pull/250)) by @fendor
- Bring in doc URL fix for Windows
([#289](https://github.com/haskell/haskell-language-server/pull/289)) by @bubba
- Bring in fix for libm on Linux static binaries
([#293](https://github.com/haskell/haskell-language-server/pull/293)) by @bubba
- Add fourmolu plugin (attempt 2) and add Brittany for ghc-8.10.1
([#264](https://github.com/haskell/haskell-language-server/pull/264)) by @georgefst
- Trying new hls-3 branch
([#300](https://github.com/haskell/haskell-language-server/pull/300)) by @alanz

## 0.2.2

This changes the configuration section from "languageServerHaskell" to "haskell"
to align it with vscode-haskell-1.0.0. Whilst the old section is still
supported for now, you should update your LSP configuration (which varies per
client) from

```json
{
  "languageServerHaskell": {
    "formattingProvider": "stylish-haskell"
  }
}
```

to

```json
{
  "haskell": {
    "formattingProvider": "stylish-haskell"
  }
}
```

### Pull requests merged for 0.2.2

- Mention docs on hover feature in README
([#209](https://github.com/haskell/haskell-language-server/pull/209)) by @georgefst
- Add static binaries for ghc-8.8.4
([#224](https://github.com/haskell/haskell-language-server/pull/224)) by @bubba
- Rename the configuration section from languageServerHaskell => haskell
([#227](https://github.com/haskell/haskell-language-server/pull/227)) by @bubba
- Use -haddock for cabal and stack
([#214](https://github.com/haskell/haskell-language-server/pull/214)) by @jneira
- slightly better shell.nix for local development
([#235](https://github.com/haskell/haskell-language-server/pull/235)) by @pepeiborra
- Shell nix further steps
([#240](https://github.com/haskell/haskell-language-server/pull/240)) by @pepeiborra
- Add numeric-version option for wrapper and server
([#241](https://github.com/haskell/haskell-language-server/pull/241)) by @fendor
- Accept the legacy "languageServerHaskell" config name
([#243](https://github.com/haskell/haskell-language-server/pull/243)) by @bubba
- Fix for Eval plugin: Error from tests not reported
([#244](https://github.com/haskell/haskell-language-server/pull/244)) by @tittoassini
- Rename binaries before uploading
([#248](https://github.com/haskell/haskell-language-server/pull/248)) by @bubba

## 0.2.1

This release includes a new eval plugin that allows Haddock code examples to be
evaluated through a code lens. For example, the code below will now offer to
evaluate `intercalate " " example`, and will insert the output in the line
below.

```haskell
example :: [String]
example = ["This is an example", "of", "interactive", "evaluation"]

-- >>> intercalate " " example
-- "This is an example of interactive evaluation"
--
```

This is also the first release to have binaries distributed alongside it. Some
behind the scene changes include the GHC library directory now being obtained on
the fly, so either `ghc`, `cabal` or `stack` will need to be present on your
PATH depending on your project. See `docs/releases.md` for more information. If
you find any issues with this, please let us know!

### Pull requests merged for 0.2.1

- Bump ormolu to 0.1.2.0
([#189](https://github.com/haskell/haskell-language-server/pull/189)) by @AlistairB
- Remove dependency on Cabal
([#195](https://github.com/haskell/haskell-language-server/pull/195)) by @bubba
- Fix extraneous extra-dep in stack-8.6.4.yaml
([#199](https://github.com/haskell/haskell-language-server/pull/199)) by @bubba
- Fix install script stack targets
([#203](https://github.com/haskell/haskell-language-server/pull/203)) by @jneira
- Add support for ghc-8.8.4
([#206](https://github.com/haskell/haskell-language-server/pull/206)) by @jneira
- Simple Eval plugin
([#191](https://github.com/haskell/haskell-language-server/pull/191)) by @pepeiborra
- Distributable binaries
([#165](https://github.com/haskell/haskell-language-server/pull/165)) by @bubba

## 0.2

- Use cabal-plan from Hackage
([#185](https://github.com/haskell/haskell-language-server/pull/185)) by @georgefst
- Bump ghcide to wz1000 hls-2 branch
([#184](https://github.com/haskell/haskell-language-server/pull/184)) by @alanz
- doc(preprocessor): Document the preprocessor limitation
([#177](https://github.com/haskell/haskell-language-server/pull/177)) by @Sir4ur0n
- Use shell.nix from Haskell-IDE-Engine
([#169](https://github.com/haskell/haskell-language-server/pull/169)) by @fendor
- Remove last occurrences of shake.yaml
([#163](https://github.com/haskell/haskell-language-server/pull/163)) by @fendor
- Use an unique install/stack.yaml
([#154](https://github.com/haskell/haskell-language-server/pull/154)) by @jneira
- Introduce golden testing
([#152](https://github.com/haskell/haskell-language-server/pull/152)) by @Ailrun
- Revert "Use bullet as separator instead of HR"
([#150](https://github.com/haskell/haskell-language-server/pull/150)) by @alanz
- feat(hie-bios): Multi-cradle, ignore directories
([#147](https://github.com/haskell/haskell-language-server/pull/147)) by @Sir4ur0n
- [Plugin] stylish-haskell formatter
([#146](https://github.com/haskell/haskell-language-server/pull/146)) by @Ailrun
- Separate ghcide tests and disable them for now
([#137](https://github.com/haskell/haskell-language-server/pull/137)) by @jneira
- Convert private lib in common stanza
([#136](https://github.com/haskell/haskell-language-server/pull/136)) by @jneira
- Add zlibc to readme
([#134](https://github.com/haskell/haskell-language-server/pull/134)) by @Sir4ur0n
- Complete editor integrations
([#132](https://github.com/haskell/haskell-language-server/pull/132)) by @jneira
- Remove inexistent component from hie.yaml.stack
([#131](https://github.com/haskell/haskell-language-server/pull/131)) by @jneira
- Bump to new mpickering/ghcide
([#130](https://github.com/haskell/haskell-language-server/pull/130)) by @alanz
- Update ghc-lib-parser version
([#129](https://github.com/haskell/haskell-language-server/pull/129)) by @jneira
- Remove redundant import
([#128](https://github.com/haskell/haskell-language-server/pull/128)) by @bubba
- Default the number of Shake threads to 0 (automatic)
([#127](https://github.com/haskell/haskell-language-server/pull/127)) by @bubba
- Added kakoune integration instructions
([#125](https://github.com/haskell/haskell-language-server/pull/125)) by @414owen
- Fix install script dev target
([#124](https://github.com/haskell/haskell-language-server/pull/124)) by @jneira
- Add plugin support for Rename providers
([#123](https://github.com/haskell/haskell-language-server/pull/123)) by @pepeiborra
- Add jobs for stack and cabal using ghc-8.10.1
([#120](https://github.com/haskell/haskell-language-server/pull/120)) by @jneira
- Add lower bound to tasty-ant-xml
([#119](https://github.com/haskell/haskell-language-server/pull/119)) by @jneira
- Fix build using brittany revision
([#117](https://github.com/haskell/haskell-language-server/pull/117)) by @jneira
- Use floskell released version 0.10.3
([#116](https://github.com/haskell/haskell-language-server/pull/116)) by @jneira
- Add emacs/doom-emacs integration sub-section
([#115](https://github.com/haskell/haskell-language-server/pull/115)) by @yuanw
- Port hie README partially
([#112](https://github.com/haskell/haskell-language-server/pull/112)) by @jneira
- Use cabal-helper-1.1, add stack-8.10.1.yaml and unify cabal.project's
([#108](https://github.com/haskell/haskell-language-server/pull/108)) by @jneira
- [#87] Fix completion via ghcide's `getCompletionsLSP`
([#107](https://github.com/haskell/haskell-language-server/pull/107)) by @korayal
- Create specific project file for ghc-8.10.
([#106](https://github.com/haskell/haskell-language-server/pull/106)) by @jneira
- Issue 5 - Move HIE Tests and convert to Tasty
([#105](https://github.com/haskell/haskell-language-server/pull/105)) by @jeffwindsor
- Hls update latest hie bios
([#100](https://github.com/haskell/haskell-language-server/pull/100)) by @fendor
- Update extra-deps to use latest fork version of shake
([#98](https://github.com/haskell/haskell-language-server/pull/98)) by @fendor
- Activate typechecking in non-lsp mode
([#95](https://github.com/haskell/haskell-language-server/pull/95)) by @jneira
- Fix haddock parsing errors
([#92](https://github.com/haskell/haskell-language-server/pull/92)) by @jneira
- Update for haskell-lsp 0.22
([#89](https://github.com/haskell/haskell-language-server/pull/89)) by @alanz
- Get building with ghc-8.10
([#83](https://github.com/haskell/haskell-language-server/pull/83)) by @bubba

## 0.1

### In this version

- cabal to 2020-05-02T10:11:15Z
- stack-8.8.3 to lts-15.10
- stack to nightly-2020-05-01

### Changes

This is the initial version, so too many to list individually.

The key point is that is now supports multi-component cradles, and has been in
daily use by the developers for some time.

It still does not have feature parity with `haskell-ide-engine`, but it is
currently useful.
