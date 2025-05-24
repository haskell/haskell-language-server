# Changelog for haskell-language-server


## 2.11.0.0

- Bindists for GHC 9.12.2
  - Full plugin support, inlcuding refactor plugin
- Bindists for GHC 9.10.2
- Bindists for GHC 9.8.4
- Bindists for GHC 9.6.7
- Bindists for GHC 9.4.8
- Dropped support for Centos 7 as this platform is no longer supported by ghc
- Improved import suggestions for contructors and OverloadedRecordDot fields

### Pull Requests

- Add doc for project-wide renaming
  ([#4584](https://github.com/haskell/haskell-language-server/pull/4584)) by @jian-lin
- Use hie-bios 0.15.0
  ([#4582](https://github.com/haskell/haskell-language-server/pull/4582)) by @fendor
- Allow building HLS with GHC 9.10.2
  ([#4581](https://github.com/haskell/haskell-language-server/pull/4581)) by @fendor
- Fix Plugin support table for 9.12.2
  ([#4580](https://github.com/haskell/haskell-language-server/pull/4580)) by @fendor
- Fix misplaced inlay hints by applying PositionMapping
  ([#4571](https://github.com/haskell/haskell-language-server/pull/4571)) by @jetjinser
- Enable hls-plugin-gadt for ghc-9.12
  ([#4568](https://github.com/haskell/haskell-language-server/pull/4568)) by @GuillaumedeVolpiano
- Remove no longer needed allow-newer
  ([#4566](https://github.com/haskell/haskell-language-server/pull/4566)) by @jhrcek
- Add missing golden files for GHC 9.10 config tests
  ([#4563](https://github.com/haskell/haskell-language-server/pull/4563)) by @jian-lin
- updating the plugins support table for refactor
  ([#4560](https://github.com/haskell/haskell-language-server/pull/4560)) by @GuillaumedeVolpiano
- Enable stylish-haskell for ghc-9.10 and ghc-9.12
  ([#4559](https://github.com/haskell/haskell-language-server/pull/4559)) by @GuillaumedeVolpiano
- Bump haskell-actions/setup from 2.7.10 to 2.7.11
  ([#4557](https://github.com/haskell/haskell-language-server/pull/4557)) by @dependabot[bot]
- Provide code action in hls-eval-plugin
  ([#4556](https://github.com/haskell/haskell-language-server/pull/4556)) by @jian-lin
- enable hlint for ghc-9.12
  ([#4555](https://github.com/haskell/haskell-language-server/pull/4555)) by @GuillaumedeVolpiano
- Enable fourmolu and ormolu for GHC 9.12
  ([#4554](https://github.com/haskell/haskell-language-server/pull/4554)) by @fendor
- Enable hls-cabal-gild-plugin for GHC 9.12.2
  ([#4553](https://github.com/haskell/haskell-language-server/pull/4553)) by @fendor
- Update plugin support table for GHC 9.12.2
  ([#4552](https://github.com/haskell/haskell-language-server/pull/4552)) by @fendor
- Remove allow-newer for hiedb
  ([#4551](https://github.com/haskell/haskell-language-server/pull/4551)) by @jhrcek
- Fix typo of rename plugin config
  ([#4546](https://github.com/haskell/haskell-language-server/pull/4546)) by @jian-lin
- Update the ghcup-metadata generation script
  ([#4545](https://github.com/haskell/haskell-language-server/pull/4545)) by @fendor
- porting hls-refactor to ghc-9.12
  ([#4543](https://github.com/haskell/haskell-language-server/pull/4543)) by @GuillaumedeVolpiano
- add ghcide-bench flag to .cabal file
  ([#4542](https://github.com/haskell/haskell-language-server/pull/4542)) by @juhp
- Revert "link executables dynamically to speed up linking (#4423)"
  ([#4541](https://github.com/haskell/haskell-language-server/pull/4541)) by @fendor
- Support PackageImports in hiddenPackageSuggestion
  ([#4537](https://github.com/haskell/haskell-language-server/pull/4537)) by @jian-lin
- Improve FreeBSD installation docs
  ([#4536](https://github.com/haskell/haskell-language-server/pull/4536)) by @arrowd
- reinstating ignore-plugins-ghc-bounds
  ([#4532](https://github.com/haskell/haskell-language-server/pull/4532)) by @GuillaumedeVolpiano
- Simplify FuzzySearch test (avoid dependency on /usr/share/dict/words)
  ([#4531](https://github.com/haskell/haskell-language-server/pull/4531)) by @jhrcek
- Import suggestion for missing newtype constructor, all types constructor and indirect overloadedrecorddot fields
  ([#4516](https://github.com/haskell/haskell-language-server/pull/4516)) by @guibou

## 2.10.0.0

- Bindists for GHC 9.12.2
  - This is only basic support, many plugins are not yet compatible.
- Bindists for GHC 9.8.4
- Bindists for GHC 9.6.7
- `hls-cabal-plugin` features
  - Support for `cabal-add`
  - Goto Definition for common sections
  - Outline of .cabal files
- Fix handling of LSP resolve requests
- Display Inlay Hints
  - Records
  - Imports

### Pull Requests

- Fix cabal check for Hackage release
  ([#4528](https://github.com/haskell/haskell-language-server/pull/4528)) by @fendor
- GHC 9.12 support
  ([#4527](https://github.com/haskell/haskell-language-server/pull/4527)) by @wz1000
- Bump cachix/install-nix-action from 30 to 31
  ([#4525](https://github.com/haskell/haskell-language-server/pull/4525)) by @dependabot[bot]
- Bump cachix/cachix-action from 15 to 16
  ([#4523](https://github.com/haskell/haskell-language-server/pull/4523)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.9 to 2.7.10
  ([#4522](https://github.com/haskell/haskell-language-server/pull/4522)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.9 to 2.7.10 in /.github/actions/setup-build
  ([#4521](https://github.com/haskell/haskell-language-server/pull/4521)) by @dependabot[bot]
- Move ghcide-test to stand alone dir
  ([#4520](https://github.com/haskell/haskell-language-server/pull/4520)) by @soulomoon
- refactor: remove unnecessary instance and use of unsafeCoerce
  ([#4518](https://github.com/haskell/haskell-language-server/pull/4518)) by @MangoIV
- convert `pre-commit-config.yaml` from JSON to YAML
  ([#4513](https://github.com/haskell/haskell-language-server/pull/4513)) by @peterbecich
- Enable bench for 9.10
  ([#4512](https://github.com/haskell/haskell-language-server/pull/4512)) by @soulomoon
- Bugfix: Explicit record fields inlay hints for polymorphic records
  ([#4510](https://github.com/haskell/haskell-language-server/pull/4510)) by @wczyz
- Capitalization of "Replace"
  ([#4509](https://github.com/haskell/haskell-language-server/pull/4509)) by @dschrempf
- document eval plugin not supporting multiline expressions
  ([#4495](https://github.com/haskell/haskell-language-server/pull/4495)) by @noughtmare
- Documentation: Imrpove "Contributing" (and amend Sphinx builders)
  ([#4494](https://github.com/haskell/haskell-language-server/pull/4494)) by @dschrempf
- Documentation: HLS plugin tutorial improvements
  ([#4491](https://github.com/haskell/haskell-language-server/pull/4491)) by @dschrempf
- Nix tooling (minor changes)
  ([#4490](https://github.com/haskell/haskell-language-server/pull/4490)) by @dschrempf
- Bump haskell-actions/setup from 2.7.8 to 2.7.9
  ([#4483](https://github.com/haskell/haskell-language-server/pull/4483)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.8 to 2.7.9 in /.github/actions/setup-build
  ([#4482](https://github.com/haskell/haskell-language-server/pull/4482)) by @dependabot[bot]
- Rework bindist CI
  ([#4481](https://github.com/haskell/haskell-language-server/pull/4481)) by @wz1000
- Remove Unsafe Dynflags deadcode, they don't exist any more!
  ([#4480](https://github.com/haskell/haskell-language-server/pull/4480)) by @fendor
- Implement fallback handler for `*/resolve` requests
  ([#4478](https://github.com/haskell/haskell-language-server/pull/4478)) by @fendor
- Bump haskell-actions/setup from 2.7.7 to 2.7.8
  ([#4477](https://github.com/haskell/haskell-language-server/pull/4477)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.7 to 2.7.8 in /.github/actions/setup-build
  ([#4476](https://github.com/haskell/haskell-language-server/pull/4476)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.6 to 2.7.7
  ([#4471](https://github.com/haskell/haskell-language-server/pull/4471)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.6 to 2.7.7 in /.github/actions/setup-build
  ([#4470](https://github.com/haskell/haskell-language-server/pull/4470)) by @dependabot[bot]
- Allow building with GHC 9.8.4
  ([#4459](https://github.com/haskell/haskell-language-server/pull/4459)) by @fendor
- Update python read-the-docs dependencies to latest
  ([#4457](https://github.com/haskell/haskell-language-server/pull/4457)) by @fendor
- More tests and better docs for cabal-add
  ([#4455](https://github.com/haskell/haskell-language-server/pull/4455)) by @VenInf
- ci(mergify): upgrade configuration to current format
  ([#4454](https://github.com/haskell/haskell-language-server/pull/4454)) by @mergify[bot]
- Support record positional construction inlay hints
  ([#4447](https://github.com/haskell/haskell-language-server/pull/4447)) by @jetjinser
- Build HLS with GHC 9.8.3
  ([#4444](https://github.com/haskell/haskell-language-server/pull/4444)) by @fendor
- Don't suggest -Wno-deferred-out-of-scope-variables
  ([#4441](https://github.com/haskell/haskell-language-server/pull/4441)) by @jeukshi
- Enable hls-stan-plugin for GHC 9.10.1
  ([#4437](https://github.com/haskell/haskell-language-server/pull/4437)) by @fendor
- Enhance formatting of the `cabal-version` error message
  ([#4436](https://github.com/haskell/haskell-language-server/pull/4436)) by @fendor
- Support structured diagnostics 2
  ([#4433](https://github.com/haskell/haskell-language-server/pull/4433)) by @noughtmare
- Cabal ignore if for completions (#4289)
  ([#4427](https://github.com/haskell/haskell-language-server/pull/4427)) by @SamuelLess
- Fix cabal-add testdata for hls-cabal-plugin-tests
  ([#4426](https://github.com/haskell/haskell-language-server/pull/4426)) by @fendor
- gracefully handle errors for unsupported cabal version
  ([#4425](https://github.com/haskell/haskell-language-server/pull/4425)) by @fridewald
- Fix pre-commit in CI
  ([#4424](https://github.com/haskell/haskell-language-server/pull/4424)) by @fendor
- link executables dynamically to speed up linking
  ([#4423](https://github.com/haskell/haskell-language-server/pull/4423)) by @develop7
- Cabal plugin: implement check for package.yaml in a stack project
  ([#4422](https://github.com/haskell/haskell-language-server/pull/4422)) by @JMoss-dev
- Fix exporting operator pattern synonym
  ([#4420](https://github.com/haskell/haskell-language-server/pull/4420)) by @pbrinkmeier
- Add docs about running tests for new contributors
  ([#4418](https://github.com/haskell/haskell-language-server/pull/4418)) by @pbrinkmeier
- Bump cachix/install-nix-action from 29 to 30
  ([#4413](https://github.com/haskell/haskell-language-server/pull/4413)) by @dependabot[bot]
- Bump cachix/install-nix-action from V27 to 29
  ([#4411](https://github.com/haskell/haskell-language-server/pull/4411)) by @dependabot[bot]
- Avoid expectFail in the test suite
  ([#4402](https://github.com/haskell/haskell-language-server/pull/4402)) by @sgillespie
- Fix typos in hls-cabal-fmt-plugin
  ([#4399](https://github.com/haskell/haskell-language-server/pull/4399)) by @fendor
- Jump to instance definition and explain typeclass evidence
  ([#4392](https://github.com/haskell/haskell-language-server/pull/4392)) by @fendor
- Update cabal-add dependency
  ([#4389](https://github.com/haskell/haskell-language-server/pull/4389)) by @VenInf
- Improve error message for `--probe-tools`
  ([#4387](https://github.com/haskell/haskell-language-server/pull/4387)) by @sgillespie
- Documentation for build-depends on hover
  ([#4385](https://github.com/haskell/haskell-language-server/pull/4385)) by @VenInf
- Bump haskell-actions/setup from 2.7.3 to 2.7.6
  ([#4384](https://github.com/haskell/haskell-language-server/pull/4384)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.5 to 2.7.6 in /.github/actions/setup-build
  ([#4383](https://github.com/haskell/haskell-language-server/pull/4383)) by @dependabot[bot]
- Clear GHCup caches in CI to not run out of space in CI
  ([#4382](https://github.com/haskell/haskell-language-server/pull/4382)) by @fendor
- Cabal go to module's definition
  ([#4380](https://github.com/haskell/haskell-language-server/pull/4380)) by @VenInf
- Add Goto Definition for cabal common sections
  ([#4375](https://github.com/haskell/haskell-language-server/pull/4375)) by @ChristophHochrainer
- cabal-add integration as a CodeAction
  ([#4360](https://github.com/haskell/haskell-language-server/pull/4360)) by @VenInf
- Bump haskell-actions/setup from 2.7.3 to 2.7.5 in /.github/actions/setup-build
  ([#4354](https://github.com/haskell/haskell-language-server/pull/4354)) by @dependabot[bot]
- Support Inlay hints for record wildcards
  ([#4351](https://github.com/haskell/haskell-language-server/pull/4351)) by @jetjinser
- Remove componentInternalUnits
  ([#4350](https://github.com/haskell/haskell-language-server/pull/4350)) by @soulomoon
- Fix core file location in `GetLinkable`
  ([#4347](https://github.com/haskell/haskell-language-server/pull/4347)) by @soulomoon
- Release 2.9.0.1
  ([#4346](https://github.com/haskell/haskell-language-server/pull/4346)) by @wz1000
- Using captureKicksDiagnostics to speed up multiple plugin tests
  ([#4339](https://github.com/haskell/haskell-language-server/pull/4339)) by @komikat
- Get files from Shake VFS from within plugin handlers
  ([#4328](https://github.com/haskell/haskell-language-server/pull/4328)) by @awjchen
- Cabal plugin outline view
  ([#4323](https://github.com/haskell/haskell-language-server/pull/4323)) by @VenInf
- Add missing documentation for cabal formatters
  ([#4322](https://github.com/haskell/haskell-language-server/pull/4322)) by @fendor
- Provide explicit import in inlay hints
  ([#4235](https://github.com/haskell/haskell-language-server/pull/4235)) by @jetjinser
- Add codeactions for cabal field names
  ([#3273](https://github.com/haskell/haskell-language-server/pull/3273)) by @dyniec

## 2.9.0.1

- Bindists for GHC 9.6.6

## 2.9.0.0

- Bindists for GHC 9.10.1 by @wz1000, @jhrcek, @michaelpj
- More hls-graph reliability improvements by @soulomoon
- Refactoring of test suite runners by @soulomoon
- Fixes in multiple home units support by @wz1000

### Pull Requests

- Fix quadratic memory usage in GetLocatedImports
  ([#4318](https://github.com/haskell/haskell-language-server/pull/4318)) by @mpickering
- Bump stack configs + CI to 9.6.5 and 9.8.2
  ([#4316](https://github.com/haskell/haskell-language-server/pull/4316)) by @jhrcek
- Add support for Fourmolu 0.16
  ([#4314](https://github.com/haskell/haskell-language-server/pull/4314)) by @ brandonchinn178
- Code action to remove redundant record field import (fixes #4220)
  ([#4308](https://github.com/haskell/haskell-language-server/pull/4308)) by @battermann
- Use restricted monad for plugins (#4057)
  ([#4304](https://github.com/haskell/haskell-language-server/pull/4304)) by @awjchen
- 4301 we need to implement utility to wait for all runnning keys in hls graph done
  ([#4302](https://github.com/haskell/haskell-language-server/pull/4302)) by @soulomoon
- Call useWithStale instead of useWithStaleFast when calling ParseCabalFields
  ([#4294](https://github.com/haskell/haskell-language-server/pull/4294)) by @VeryMilkyJoe
- test: add test documenting #806
  ([#4292](https://github.com/haskell/haskell-language-server/pull/4292)) by @develop7
- ghcide: drop ghc-check and ghc-paths dependency
  ([#4291](https://github.com/haskell/haskell-language-server/pull/4291)) by @wz1000
- Limit number of valid hole fits to 10
  ([#4288](https://github.com/haskell/haskell-language-server/pull/4288)) by @akshaymankar
- Add common stanza to completion data
  ([#4286](https://github.com/haskell/haskell-language-server/pull/4286)) by @VeryMilkyJoe
- FindImports: ThisPkg means some home unit, not "this" unit
  ([#4284](https://github.com/haskell/haskell-language-server/pull/4284)) by @wz1000
- Remove redudant absolutization in session loader
  ([#4280](https://github.com/haskell/haskell-language-server/pull/4280)) by @soulomoon
- Bump to new lsp versions
  ([#4279](https://github.com/haskell/haskell-language-server/pull/4279)) by @michaelpj
- Put more test code into pre-commit
  ([#4275](https://github.com/haskell/haskell-language-server/pull/4275)) by @soulomoon
- Delete library ghcide test utils
  ([#4274](https://github.com/haskell/haskell-language-server/pull/4274)) by @soulomoon
- Delete testUtil from ghcide-tests
  ([#4272](https://github.com/haskell/haskell-language-server/pull/4272)) by @soulomoon
- CI change, only run bench on performance label
  ([#4271](https://github.com/haskell/haskell-language-server/pull/4271)) by @soulomoon
- Migrate WatchedFileTests
  ([#4269](https://github.com/haskell/haskell-language-server/pull/4269)) by @soulomoon
- Migrate UnitTests
  ([#4268](https://github.com/haskell/haskell-language-server/pull/4268)) by @soulomoon
- Migrate SafeTests
  ([#4267](https://github.com/haskell/haskell-language-server/pull/4267)) by @soulomoon
- Migrate SymlinkTests
  ([#4266](https://github.com/haskell/haskell-language-server/pull/4266)) by @soulomoon
- Remove unused and outdated CHANGELOG files
  ([#4264](https://github.com/haskell/haskell-language-server/pull/4264)) by @fendor
- Enable cabal flaky test
  ([#4263](https://github.com/haskell/haskell-language-server/pull/4263)) by @soulomoon
- Migrate RootUriTests
  ([#4261](https://github.com/haskell/haskell-language-server/pull/4261)) by @soulomoon
- Migrate PreprocessorTests
  ([#4260](https://github.com/haskell/haskell-language-server/pull/4260)) by @soulomoon
- Migrate PluginSimpleTests
  ([#4259](https://github.com/haskell/haskell-language-server/pull/4259)) by @soulomoon
- Migrate ClientSettingsTests
  ([#4258](https://github.com/haskell/haskell-language-server/pull/4258)) by @soulomoon
- Unify critical session running in hls
  ([#4256](https://github.com/haskell/haskell-language-server/pull/4256)) by @soulomoon
- Bump cachix/cachix-action from 14 to 15
  ([#4255](https://github.com/haskell/haskell-language-server/pull/4255)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.2 to 2.7.3
  ([#4254](https://github.com/haskell/haskell-language-server/pull/4254)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.2 to 2.7.3 in /.github/actions/setup-build
  ([#4253](https://github.com/haskell/haskell-language-server/pull/4253)) by @dependabot[bot]
- Shorter file names completion
  ([#4252](https://github.com/haskell/haskell-language-server/pull/4252)) by @VenInf
- Fix progress start delay
  ([#4249](https://github.com/haskell/haskell-language-server/pull/4249)) by @michaelpj
- Bump cachix/install-nix-action from 26 to 27
  ([#4245](https://github.com/haskell/haskell-language-server/pull/4245)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.1 to 2.7.2
  ([#4244](https://github.com/haskell/haskell-language-server/pull/4244)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.1 to 2.7.2 in /.github/actions/setup-build
  ([#4243](https://github.com/haskell/haskell-language-server/pull/4243)) by @dependabot[bot]
- Enable test for #717
  ([#4241](https://github.com/haskell/haskell-language-server/pull/4241)) by @soulomoon
- Remove Pepe from CODEOWNERS
  ([#4239](https://github.com/haskell/haskell-language-server/pull/4239)) by @michaelpj
- Fix resultBuilt(dirty mechanism) in hls-graph
  ([#4238](https://github.com/haskell/haskell-language-server/pull/4238)) by @soulomoon
- Support for 9.10
  ([#4233](https://github.com/haskell/haskell-language-server/pull/4233)) by @wz1000
- Refactor hls-test-util and reduce getCurrentDirectory after initilization
  ([#4231](https://github.com/haskell/haskell-language-server/pull/4231)) by @soulomoon
- [Migrate BootTests] part of #4173 Migrate ghcide tests to hls test utils
  ([#4227](https://github.com/haskell/haskell-language-server/pull/4227)) by @soulomoon
- Actually enable pedantic flag in ci flags job
  ([#4224](https://github.com/haskell/haskell-language-server/pull/4224)) by @jhrcek
- Cleanup cabal files, ghc compat code, fix ghc warnings
  ([#4222](https://github.com/haskell/haskell-language-server/pull/4222)) by @jhrcek
- Another attempt at using the lsp API for some progress reporting
  ([#4218](https://github.com/haskell/haskell-language-server/pull/4218)) by @michaelpj
- [Migrate diagnosticTests] part of #4173 Migrate ghcide tests to hls test utils
  ([#4207](https://github.com/haskell/haskell-language-server/pull/4207)) by @soulomoon
- Prepare release 2.8.0.0
  ([#4191](https://github.com/haskell/haskell-language-server/pull/4191)) by @wz1000
- Stabilize the build system by correctly house keeping the dirtykeys and rule values [flaky test #4185 #4093]
  ([#4190](https://github.com/haskell/haskell-language-server/pull/4190)) by @soulomoon
- hls-cabal-plugin: refactor context search to use `readFields`
  ([#4186](https://github.com/haskell/haskell-language-server/pull/4186)) by @fendor
- 3944 extend the properties api to better support nested configuration
  ([#3952](https://github.com/haskell/haskell-language-server/pull/3952)) by @soulomoon

## 2.8.0.0

- Bindists for GHC 9.6.5
- New hls-notes plugin (#4126, @jvanbruegge)
- Floskell, hlint and stylish-haskell plugins enabled for GHC 9.8
- Improvements for hls-graph increasing robustness (#4087, @soulomoon)
- Improvements to multi-component support (#4096, #4109, #4179, @wz1000, @fendor)

### Pull Requests

- Bump haskell-actions/setup from 2.7.0 to 2.7.1
  ([#4189](https://github.com/haskell/haskell-language-server/pull/4189)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.7.0 to 2.7.1 in /.github/actions/setup-build
  ([#4188](https://github.com/haskell/haskell-language-server/pull/4188)) by @dependabot[bot]
- Fix ghcdie-tests CI
  ([#4184](https://github.com/haskell/haskell-language-server/pull/4184)) by @soulomoon
- Fix ghc and hlint warnings, fix formatting
  ([#4181](https://github.com/haskell/haskell-language-server/pull/4181)) by @jhrcek
- Allow users to specify whether to use `cabal`'s multi-repl feature
  ([#4179](https://github.com/haskell/haskell-language-server/pull/4179)) by @fendor
- Improve parsing of import suggestions extending multiple multiline imports (fixes #4175)
  ([#4177](https://github.com/haskell/haskell-language-server/pull/4177)) by @jhrcek
- move ghcide-tests to haskell-language-server.cabal and make it depend on hls-test-utils
  ([#4176](https://github.com/haskell/haskell-language-server/pull/4176)) by @soulomoon
- enable ThreadId for when testing
  ([#4174](https://github.com/haskell/haskell-language-server/pull/4174)) by @soulomoon
- Drop Legacy Logger from Codebase
  ([#4171](https://github.com/haskell/haskell-language-server/pull/4171)) by @fendor
- get rid of the `unsafeInterleaveIO` at start up
  ([#4167](https://github.com/haskell/haskell-language-server/pull/4167)) by @soulomoon
- Remove EKG
  ([#4163](https://github.com/haskell/haskell-language-server/pull/4163)) by @michaelpj
- Mark plugins as not buildable if the flag is disabled
  ([#4160](https://github.com/haskell/haskell-language-server/pull/4160)) by @michaelpj
- Fix references to old CPP names in tests, update tests
  ([#4159](https://github.com/haskell/haskell-language-server/pull/4159)) by @jhrcek
- Bump haskell-actions/setup from 2.6.3 to 2.7.0
  ([#4158](https://github.com/haskell/haskell-language-server/pull/4158)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.6.3 to 2.7.0 in /.github/actions/setup-build
  ([#4157](https://github.com/haskell/haskell-language-server/pull/4157)) by @dependabot[bot]
- Remove dead code in ghcide and hls-graph for priority
  ([#4151](https://github.com/haskell/haskell-language-server/pull/4151)) by @soulomoon
- Bump haskell-actions/setup from 2.6.2 to 2.6.3 in /.github/actions/setup-build
  ([#4150](https://github.com/haskell/haskell-language-server/pull/4150)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.6.2 to 2.6.3
  ([#4149](https://github.com/haskell/haskell-language-server/pull/4149)) by @dependabot[bot]
- Run ExceptionTests in temporary directory
  ([#4146](https://github.com/haskell/haskell-language-server/pull/4146)) by @fendor
- hls-eval-plugin: Replicate #4139
  ([#4140](https://github.com/haskell/haskell-language-server/pull/4140)) by @mattapet
- Update comment in refactor tests
  ([#4138](https://github.com/haskell/haskell-language-server/pull/4138)) by @jhrcek
- Update contact info in docs
  ([#4137](https://github.com/haskell/haskell-language-server/pull/4137)) by @jhrcek
- hls-notes-plugin: Do not error if no note is under the cursor
  ([#4136](https://github.com/haskell/haskell-language-server/pull/4136)) by @jvanbruegge
- improve logging in semantic tokens rule
  ([#4135](https://github.com/haskell/haskell-language-server/pull/4135)) by @soulomoon
- Bump softprops/action-gh-release from 1 to 2
  ([#4133](https://github.com/haskell/haskell-language-server/pull/4133)) by @dependabot[bot]
- Bump cachix/install-nix-action from 25 to 26
  ([#4132](https://github.com/haskell/haskell-language-server/pull/4132)) by @dependabot[bot]
- Use Set.member instead of Foldable.elem
  ([#4128](https://github.com/haskell/haskell-language-server/pull/4128)) by @jhrcek
- hls-notes-plugin: Initial implementation
  ([#4126](https://github.com/haskell/haskell-language-server/pull/4126)) by @jvanbruegge
- Enable floskell and hlint plugins for ghc 9.8
  ([#4125](https://github.com/haskell/haskell-language-server/pull/4125)) by @jhrcek
- Integrate stylish-haskell into hls executable with ghc 9.8
  ([#4124](https://github.com/haskell/haskell-language-server/pull/4124)) by @jhrcek
- Reduce usage of partial functions
  ([#4123](https://github.com/haskell/haskell-language-server/pull/4123)) by @jhrcek
- Benchmark: Enable 9.6, 9.8
  ([#4118](https://github.com/haskell/haskell-language-server/pull/4118)) by @soulomoon
- Bump haskell-actions/setup from 2.6.1 to 2.6.2 in /.github/actions/setup-build
  ([#4116](https://github.com/haskell/haskell-language-server/pull/4116)) by @dependabot[bot]
- Bump haskell-actions/setup from 2.6.1 to 2.6.2
  ([#4115](https://github.com/haskell/haskell-language-server/pull/4115)) by @dependabot[bot]
- eval: more robust way to extract comments from ParsedModule
  ([#4113](https://github.com/haskell/haskell-language-server/pull/4113)) by @jhrcek
- Improve isolation of build artefacts of test runs
  ([#4112](https://github.com/haskell/haskell-language-server/pull/4112)) by @fendor
- Improve handling of nonsense rename attempts
  ([#4111](https://github.com/haskell/haskell-language-server/pull/4111)) by @jhrcek
- Exit with non-zero exitcode if wrapper fails to launch
  ([#4110](https://github.com/haskell/haskell-language-server/pull/4110)) by @fendor
- Replace checkHomeUnitsClosed with a faster implementation
  ([#4109](https://github.com/haskell/haskell-language-server/pull/4109)) by @wz1000
- Don't distribute gifs or plugin readmes
  ([#4107](https://github.com/haskell/haskell-language-server/pull/4107)) by @fendor
- Remove locale workaround for Module name that conatins non-ascii characters
  ([#4106](https://github.com/haskell/haskell-language-server/pull/4106)) by @fendor
- Track extra-source-files of plugins more accurately
  ([#4105](https://github.com/haskell/haskell-language-server/pull/4105)) by @fendor
- remove non-ascii name
  ([#4103](https://github.com/haskell/haskell-language-server/pull/4103)) by @soulomoon
- Add cabal-gild as a cabal file formatter plugin
  ([#4101](https://github.com/haskell/haskell-language-server/pull/4101)) by @fendor
- Remove more workarounds for GHCs < 9.2 (#4092)
  ([#4098](https://github.com/haskell/haskell-language-server/pull/4098)) by @jhrcek
- session-loader: Don't loop forever when we don't find a file in any multi component
  ([#4096](https://github.com/haskell/haskell-language-server/pull/4096)) by @wz1000
- Prepare release 2.7.0.0
  ([#4095](https://github.com/haskell/haskell-language-server/pull/4095)) by @fendor
- Remove more workarounds for GHCs < 9.0
  ([#4092](https://github.com/haskell/haskell-language-server/pull/4092)) by @jhrcek
- Fix hls-graph: phantom dependencies invoke in branching deps (resolve #3423)
  ([#4087](https://github.com/haskell/haskell-language-server/pull/4087)) by @soulomoon
- Rename only if the current module compiles (#3799)
  ([#3848](https://github.com/haskell/haskell-language-server/pull/3848)) by @sgillespie
- Reintroduce ghc-lib flag for hlint plugin
  ([#3757](https://github.com/haskell/haskell-language-server/pull/3757)) by @RaoulHC

## 2.7.0.0

- Bindists for GHC 9.8.2
  - Enable many more plugins, making GHC 9.8.2 fully supported
- Fix refactor code actions for vim
- Preserve HLint's diagnostic severity
- Many other bug fixes.

### Pull Requests

- Enable pedantic for remaining plugins
  ([#4091](https://github.com/haskell/haskell-language-server/pull/4091)) by @jhrcek
- Add support for fourmolu 0.15
  ([#4086](https://github.com/haskell/haskell-language-server/pull/4086)) by @brandonchinn178
- refactor plugin: fix regex for extracting import suggestions
  ([#4080](https://github.com/haskell/haskell-language-server/pull/4080)) by @jhrcek
- Bump to hiedb 0.6.0.0
  ([#4077](https://github.com/haskell/haskell-language-server/pull/4077)) by @jhrcek
- ghcide: Only try `stat`ing a core file after we ensure it actually exists
  ([#4076](https://github.com/haskell/haskell-language-server/pull/4076)) by @wz1000
- Fix small typo in Retrie error message
  ([#4075](https://github.com/haskell/haskell-language-server/pull/4075)) by @iustin
- add Method_TextDocumentSemanticTokensFullDelta
  ([#4073](https://github.com/haskell/haskell-language-server/pull/4073)) by @soulomoon
- Fix -Wall in retrie plugin
  ([#4071](https://github.com/haskell/haskell-language-server/pull/4071)) by @jhrcek
- Fix -Wall in qualified imported names plugin
  ([#4070](https://github.com/haskell/haskell-language-server/pull/4070)) by @jhrcek
- benchmarks: switch from deprecated haskell/actions/setup to haskell-actions/setup
  ([#4068](https://github.com/haskell/haskell-language-server/pull/4068)) by @jhrcek
- Bump pre-commit/action from 3.0.0 to 3.0.1
  ([#4066](https://github.com/haskell/haskell-language-server/pull/4066)) by @dependabot[bot]
- Fix -Wall in refactor plugin
  ([#4065](https://github.com/haskell/haskell-language-server/pull/4065)) by @jhrcek
- Redundant imports/exports: use range only to determine which code actions are in scope
  ([#4063](https://github.com/haskell/haskell-language-server/pull/4063)) by @keithfancher
- Bump haskell-actions/setup to get GHC 9.6.4 in CI
  ([#4062](https://github.com/haskell/haskell-language-server/pull/4062)) by @jhrcek
- Enable pedantic for more components
  ([#4061](https://github.com/haskell/haskell-language-server/pull/4061)) by @jhrcek
- stack CI: switch to offic. haskell images, bump to lts-22.9 (ghc 9.6.4)
  ([#4060](https://github.com/haskell/haskell-language-server/pull/4060)) by @jhrcek
- Improve hls class plugin test
  ([#4059](https://github.com/haskell/haskell-language-server/pull/4059)) by @soulomoon
- Bump ghcide-test-utils to 2.0.0.0
  ([#4058](https://github.com/haskell/haskell-language-server/pull/4058)) by @wz1000
- Promote more warnings to errors in ghcide
  ([#4054](https://github.com/haskell/haskell-language-server/pull/4054)) by @jhrcek
- Add -Wunused-packages to common warnings
  ([#4053](https://github.com/haskell/haskell-language-server/pull/4053)) by @jhrcek
- Bump lsp versions
  ([#4052](https://github.com/haskell/haskell-language-server/pull/4052)) by @michaelpj
- Optimize semantic token extraction logic
  ([#4050](https://github.com/haskell/haskell-language-server/pull/4050)) by @soulomoon
- Fix warnings in hls-graph, enable pedantic in CI
  ([#4047](https://github.com/haskell/haskell-language-server/pull/4047)) by @jhrcek
- Fix -Wredundant-constraints
  ([#4044](https://github.com/haskell/haskell-language-server/pull/4044)) by @jhrcek
- Disable caching job with ghc 9.2 on windows
  ([#4043](https://github.com/haskell/haskell-language-server/pull/4043)) by @jhrcek
- fix token omitting problem if multiple tokens are connected.
  ([#4041](https://github.com/haskell/haskell-language-server/pull/4041)) by @soulomoon
- Set test options via cabal.project
  ([#4039](https://github.com/haskell/haskell-language-server/pull/4039)) by @michaelpj
- Fix document version test in hls-class-plugin
  ([#4038](https://github.com/haskell/haskell-language-server/pull/4038)) by @July541
- Fix -Wunused-imports
  ([#4037](https://github.com/haskell/haskell-language-server/pull/4037)) by @jhrcek
- Use GHC2021
  ([#4033](https://github.com/haskell/haskell-language-server/pull/4033)) by @michaelpj
- Remove ghcide-test-utils as a separate package
  ([#4032](https://github.com/haskell/haskell-language-server/pull/4032)) by @michaelpj
- Fix weird behavior of OPTIONS_GHC completions (fixes #3908)
  ([#4031](https://github.com/haskell/haskell-language-server/pull/4031)) by @jhrcek
- semantic tokens: add infix operator
  ([#4030](https://github.com/haskell/haskell-language-server/pull/4030)) by @soulomoon
- fix: a typo in docs/configuration.md
  ([#4029](https://github.com/haskell/haskell-language-server/pull/4029)) by @kkweon
- Turn off tasty-rerun
  ([#4028](https://github.com/haskell/haskell-language-server/pull/4028)) by @michaelpj
- Reduce the number of ad-hoc helper test functions in refactor plugin tests
  ([#4027](https://github.com/haskell/haskell-language-server/pull/4027)) by @jhrcek
- Fix documentation/image links
  ([#4025](https://github.com/haskell/haskell-language-server/pull/4025)) by @jhrcek
- Fix various issues
  ([#4024](https://github.com/haskell/haskell-language-server/pull/4024)) by @michaelpj
- Use relative file paths for HIE files and Stan's config maps
  ([#4023](https://github.com/haskell/haskell-language-server/pull/4023)) by @keithfancher
- fix isClassNodeIdentifier in hls-class-plugin
  ([#4020](https://github.com/haskell/haskell-language-server/pull/4020)) by @soulomoon
- Fix -Wall and -Wunused-packages in hlint plugin
  ([#4019](https://github.com/haskell/haskell-language-server/pull/4019)) by @jhrcek
- update hlint to 3.8 and prevent linting on testdata dir
  ([#4018](https://github.com/haskell/haskell-language-server/pull/4018)) by @soulomoon
- refactor plugin: add reproducer and fix for #3795
  ([#4016](https://github.com/haskell/haskell-language-server/pull/4016)) by @jhrcek
- Fix -Wall and -Wunused-packages in stylish-haskell plugin
  ([#4015](https://github.com/haskell/haskell-language-server/pull/4015)) by @jhrcek
- Fix -Wall and -Wunused-packages in stan plugin
  ([#4014](https://github.com/haskell/haskell-language-server/pull/4014)) by @jhrcek
- fix doc for semantic token
  ([#4011](https://github.com/haskell/haskell-language-server/pull/4011)) by @soulomoon
- Fix -Wall and -Wunused-packages in module name and overloaded record dot plugins
  ([#4009](https://github.com/haskell/haskell-language-server/pull/4009)) by @jhrcek
- Fix -Wall and -Wunused-package in gadt plugin
  ([#4008](https://github.com/haskell/haskell-language-server/pull/4008)) by @jhrcek
- Fix -Wall and -Wunused-packages in fourmolu and ormolu plugins
  ([#4007](https://github.com/haskell/haskell-language-server/pull/4007)) by @jhrcek
- Fix -Wall and -Wunused-packages in plugins api and floskell
  ([#4005](https://github.com/haskell/haskell-language-server/pull/4005)) by @jhrcek
- Fix -Wunused-packages in test utils
  ([#4004](https://github.com/haskell/haskell-language-server/pull/4004)) by @jhrcek
- Update base lower bounds for HLS
  ([#4000](https://github.com/haskell/haskell-language-server/pull/4000)) by @fendor
- Various 9.8 compat
  ([#3998](https://github.com/haskell/haskell-language-server/pull/3998)) by @michaelpj
- Fix -Wall and -Wunused-packages in explicit-record-fields plugin
  ([#3996](https://github.com/haskell/haskell-language-server/pull/3996)) by @jhrcek
- Fix -Wall and -Wunused-packages in explicit fixity plugin
  ([#3995](https://github.com/haskell/haskell-language-server/pull/3995)) by @jhrcek
- Remove an allow-newer
  ([#3989](https://github.com/haskell/haskell-language-server/pull/3989)) by @michaelpj
- chore: Fix typo s/occured/occurred
  ([#3988](https://github.com/haskell/haskell-language-server/pull/3988)) by @hugo-syn
- Update support tables
  ([#3987](https://github.com/haskell/haskell-language-server/pull/3987)) by @michaelpj
- Fix most -Wall in ghcide
  ([#3984](https://github.com/haskell/haskell-language-server/pull/3984)) by @jhrcek
- Fix -Wall and -Wunused-packages in pragmas plugin
  ([#3982](https://github.com/haskell/haskell-language-server/pull/3982)) by @jhrcek
- Fix -Wall and -Wunused-packages in eval plugin
  ([#3981](https://github.com/haskell/haskell-language-server/pull/3981)) by @jhrcek
- Fix -Wall and -Wunused-packages in code-range plugin
  ([#3980](https://github.com/haskell/haskell-language-server/pull/3980)) by @jhrcek
- Fix -Wall, -Wunused-packages and hlint warnings in call-hierarchy plugin
  ([#3979](https://github.com/haskell/haskell-language-server/pull/3979)) by @jhrcek
- Fix -Wunused-packages in hls-cabal-plugin
  ([#3977](https://github.com/haskell/haskell-language-server/pull/3977)) by @jhrcek
- Merge plugins into the HLS package
  ([#3976](https://github.com/haskell/haskell-language-server/pull/3976)) by @michaelpj
- Fix most hlint warnings in ghcide
  ([#3975](https://github.com/haskell/haskell-language-server/pull/3975)) by @jhrcek
- Remove allow-newer for ghc-trace-events
  ([#3974](https://github.com/haskell/haskell-language-server/pull/3974)) by @jhrcek
- Exactprint plugins for 9.8
  ([#3973](https://github.com/haskell/haskell-language-server/pull/3973)) by @wz1000
- Fix -Wall and -Wunused-packages in hls-class-plugin
  ([#3972](https://github.com/haskell/haskell-language-server/pull/3972)) by @jhrcek
- Document cabal diagnostic options
  ([#3971](https://github.com/haskell/haskell-language-server/pull/3971)) by @fendor
- Fix -Wall and -Wunused-packages in change-type-signature plugin
  ([#3970](https://github.com/haskell/haskell-language-server/pull/3970)) by @jhrcek
- Semantic tokens: expand type synonym to checkout forall function type when possible
  ([#3967](https://github.com/haskell/haskell-language-server/pull/3967)) by @soulomoon
- Fix -Wunused-packages in hls-cabal-fmt-plugin
  ([#3965](https://github.com/haskell/haskell-language-server/pull/3965)) by @jhrcek
- Fix -Wall and -Wunused-packages in hls-alternate-number-format-plugin
  ([#3964](https://github.com/haskell/haskell-language-server/pull/3964)) by @jhrcek
- Prepare release 2.6.0.0
  ([#3959](https://github.com/haskell/haskell-language-server/pull/3959)) by @wz1000
- Semantic tokens: add module name support and improve performance and accuracy by traversing the hieAst along with source code
  ([#3958](https://github.com/haskell/haskell-language-server/pull/3958)) by @soulomoon
- Bump cachix/cachix-action from 13 to 14
  ([#3956](https://github.com/haskell/haskell-language-server/pull/3956)) by @dependabot[bot]
- Bump cachix/install-nix-action from 24 to 25
  ([#3955](https://github.com/haskell/haskell-language-server/pull/3955)) by @dependabot[bot]
- Remove unused dependencies in hls-refactor-plugin
  ([#3953](https://github.com/haskell/haskell-language-server/pull/3953)) by @jhrcek
- Cleanup conditional build logic pertaining to pre 9.2 GHCs
  ([#3948](https://github.com/haskell/haskell-language-server/pull/3948)) by @jhrcek
- Fix issue:  HLS HLint plugin doesn't preserve HLint's severities #3881
  ([#3902](https://github.com/haskell/haskell-language-server/pull/3902)) by @IAmPara0x
- Don't run hlint on testdata directories
  ([#3901](https://github.com/haskell/haskell-language-server/pull/3901)) by @fendor
-  Add option for setting manual path to Fourmolu binary
  ([#3860](https://github.com/haskell/haskell-language-server/pull/3860)) by @georgefst

## 2.6.0.0

- Bindists for GHC 9.6.4
- A new semantic tokens plugin (#3892, @soulomoon).
- Improvements to multiple home unit support with GHC 9.4. When HLS is used with cabal 3.11+ it will
  load proper multiple home unit sessions by default, fixing a lot of issues with
  loading and reloading projects that have more than one component (#3462, @wz1000).
- Removed implicit-hie, resulting in better behaviour for projects without cradles.
- Don't produce diagnostics for disabled plugins (#3941, @fendor).
- Many other bug fixes.

### Pull Requests

- fix: semantic token omitting record field in `{-# LANGUAGE DuplicateRecordFields  #-}` #3950
  ([#3951](https://github.com/haskell/haskell-language-server/pull/3951)) by @soulomoon
- Properties API: Remove unsafe coerce in favor of type class based method in
  ([#3947](https://github.com/haskell/haskell-language-server/pull/3947)) by @soulomoon
- Bump to hiedb 0.5.0.0 to fix #3542
  ([#3943](https://github.com/haskell/haskell-language-server/pull/3943)) by @wz1000
- Don't produce diagnostics if plugin is turned off
  ([#3941](https://github.com/haskell/haskell-language-server/pull/3941)) by @fendor
- add config  for semantic-tokens-plugin for mapping from hs token type to LSP default token type
  ([#3940](https://github.com/haskell/haskell-language-server/pull/3940)) by @soulomoon
- add doc and ci test for semantic tokens
  ([#3938](https://github.com/haskell/haskell-language-server/pull/3938)) by @soulomoon
- update Floskell to 0.11.*
  ([#3933](https://github.com/haskell/haskell-language-server/pull/3933)) by @peterbecich
- Remove some people from CODEOWNERS
  ([#3930](https://github.com/haskell/haskell-language-server/pull/3930)) by @michaelpj
- Adapt to minor API change for 9.6.4 compatibility
  ([#3929](https://github.com/haskell/haskell-language-server/pull/3929)) by @wz1000
- Fix multi unit session when some packages have reexported modules.
  ([#3928](https://github.com/haskell/haskell-language-server/pull/3928)) by @wz1000
- Switch to haskell-actions/setup since haskell/actions is deprecated
  ([#3926](https://github.com/haskell/haskell-language-server/pull/3926)) by @fendor
- Make vscode-extension-schema honour default values
  ([#3925](https://github.com/haskell/haskell-language-server/pull/3925)) by @fendor
- Add golden tests for public configs
  ([#3922](https://github.com/haskell/haskell-language-server/pull/3922)) by @fendor
- Bump geekyeggo/delete-artifact from 2 to 4
  ([#3921](https://github.com/haskell/haskell-language-server/pull/3921)) by @dependabot[bot]
- Fix positionMapping in stale data
  ([#3920](https://github.com/haskell/haskell-language-server/pull/3920)) by @soulomoon
- Disable stan plugin by default
  ([#3917](https://github.com/haskell/haskell-language-server/pull/3917)) by @fendor
- Use stan config files for stan plugin (#3904)
  ([#3914](https://github.com/haskell/haskell-language-server/pull/3914)) by @0rphee
- Bump both upload and download artifact
  ([#3913](https://github.com/haskell/haskell-language-server/pull/3913)) by @michaelpj
- Update ghc-version-support.md for 2.5.0
  ([#3909](https://github.com/haskell/haskell-language-server/pull/3909)) by @lehmacdj
- Give plugins descriptions, include versions of key dependencies
  ([#3903](https://github.com/haskell/haskell-language-server/pull/3903)) by @michaelpj
- Remove some buildability blockers that aren't needed
  ([#3899](https://github.com/haskell/haskell-language-server/pull/3899)) by @michaelpj
- Bump actions/setup-python from 4 to 5
  ([#3895](https://github.com/haskell/haskell-language-server/pull/3895)) by @dependabot[bot]
- Update index-state to get latest stan version
  ([#3894](https://github.com/haskell/haskell-language-server/pull/3894)) by @0rphee
- Generate FileTarget for all possible targetLocations
  ([#3893](https://github.com/haskell/haskell-language-server/pull/3893)) by @fendor
- Implement semantic tokens plugin to support semantic highlighting(textDocument/semanticTokens/full)
  ([#3892](https://github.com/haskell/haskell-language-server/pull/3892)) by @soulomoon
- session-loader: Set working directory on GHC 9.4+
  ([#3891](https://github.com/haskell/haskell-language-server/pull/3891)) by @wz1000
- Demote home unit closure errors to warnings.
  ([#3890](https://github.com/haskell/haskell-language-server/pull/3890)) by @wz1000
- Bump cachix/install-nix-action from 23 to 24
  ([#3889](https://github.com/haskell/haskell-language-server/pull/3889)) by @dependabot[bot]
- Bump cachix/cachix-action from 12 to 13
  ([#3888](https://github.com/haskell/haskell-language-server/pull/3888)) by @dependabot[bot]
- Add more docs for implicit discovery
  ([#3887](https://github.com/haskell/haskell-language-server/pull/3887)) by @fendor
- Prepare release 2.5.0.0
  ([#3879](https://github.com/haskell/haskell-language-server/pull/3879)) by @wz1000
- Improve no plugin messages
  ([#3864](https://github.com/haskell/haskell-language-server/pull/3864)) by @joyfulmantis
- Add support for multi unit argument syntax
  ([#3462](https://github.com/haskell/haskell-language-server/pull/3462)) by @wz1000
- Fix completion for qualified import
  ([#2838](https://github.com/haskell/haskell-language-server/pull/2838)) by @xsebek

## 2.5.0.0

- Bindists for GHC 9.4.8
- Drop support for GHC 9.0
- Re-add stan plugin
- Load default operator fixities in Fourmolu plugin

### Pull Requests

- Drop support for GHC 9.0
  ([#3875](https://github.com/haskell/haskell-language-server/pull/3875)) by @michaelpj
- Fix support tables
  ([#3874](https://github.com/haskell/haskell-language-server/pull/3874)) by @michaelpj
- Prefer hls-test-utils functions over code duplication
  ([#3870](https://github.com/haskell/haskell-language-server/pull/3870)) by @fendor
- Make sure running tests locally pick up the correct cradle type
  ([#3869](https://github.com/haskell/haskell-language-server/pull/3869)) by @fendor
- Some versions of stylish-haskell do need the ghc-lib flag
  ([#3868](https://github.com/haskell/haskell-language-server/pull/3868)) by @michaelpj
- Remove head.hackage
  ([#3867](https://github.com/haskell/haskell-language-server/pull/3867)) by @wz1000
- Load default operator fixities in Fourmolu plugin non-CLI mode
  ([#3855](https://github.com/haskell/haskell-language-server/pull/3855)) by @georgefst
- Fix #3847
  ([#3854](https://github.com/haskell/haskell-language-server/pull/3854)) by @BurningLutz
- Re-add hls-stan-plugin
  ([#3851](https://github.com/haskell/haskell-language-server/pull/3851)) by @0rphee
- Bump fkirc/skip-duplicate-actions from 5.3.0 to 5.3.1
  ([#3850](https://github.com/haskell/haskell-language-server/pull/3850)) by @dependabot[bot]
- Merge definitions from all plugins for Document(Type)Definition message
  ([#3846](https://github.com/haskell/haskell-language-server/pull/3846)) by @JiriLojda
- Simplify cabal.project
  ([#3836](https://github.com/haskell/haskell-language-server/pull/3836)) by @michaelpj
- Set the root for tests to the test directory
  ([#3830](https://github.com/haskell/haskell-language-server/pull/3830)) by @fendor
- Reduce Nix support
  ([#3804](https://github.com/haskell/haskell-language-server/pull/3804)) by @michaelpj

## 2.4.0.0

* Initial support for GHC 9.8.1, without plugins dependent on `ghc-exactprint`
* Fix broken Windows binaries (#3822)

### Pull Requests

- Remove constraint on stm-hamt
  ([#3829](https://github.com/haskell/haskell-language-server/pull/3829)) by @iMichka
- Cleanup func-test suite
  ([#3828](https://github.com/haskell/haskell-language-server/pull/3828)) by @fendor
- Bump haskell/actions from 2.4.6 to 2.4.7 in /.github/actions/setup-build
  ([#3824](https://github.com/haskell/haskell-language-server/pull/3824)) by @dependabot[bot]
- Bump haskell/actions from 2.4.6 to 2.4.7
  ([#3823](https://github.com/haskell/haskell-language-server/pull/3823)) by @dependabot[bot]
- Release 2.3.0.0
  ([#3818](https://github.com/haskell/haskell-language-server/pull/3818)) by @wz1000
- GHC 9.8 support
  ([#3727](https://github.com/haskell/haskell-language-server/pull/3727)) by @wz1000

## 2.3.0.0

* Binaries for GHC 9.6.3
* Drop support for GHC 8.10
* Remove `hls-haddock-comments-plugin`, `hls-stan-plugin`, and `hls-tactics-plugin`
* Don't suggest bogus modules names in `hls-module-name-plugin` (#3784)
* Add support for external Ormolu (#3771)
* Improve refine imports behaviour for qualified imports (#3806)

### Pull Requests

- Switch chat room to matrix
  ([#3817](https://github.com/haskell/haskell-language-server/pull/3817)) by @July541
- Fix flaky hie bios test
  ([#3814](https://github.com/haskell/haskell-language-server/pull/3814)) by @fendor
- Revert "Bump actions/checkout from 3 to 4"
  ([#3813](https://github.com/haskell/haskell-language-server/pull/3813)) by @wz1000
- Add test directories to hls-retrie-plugin
  ([#3808](https://github.com/haskell/haskell-language-server/pull/3808)) by @Vekhir
- Change refine imports behaviour for qualified imports
  ([#3806](https://github.com/haskell/haskell-language-server/pull/3806)) by @joyfulmantis
- Update links to Nix documentation
  ([#3805](https://github.com/haskell/haskell-language-server/pull/3805)) by @maralorn
- Bump actions/checkout from 3 to 4
  ([#3802](https://github.com/haskell/haskell-language-server/pull/3802)) by @dependabot[bot]
- Bump cachix/install-nix-action from 22 to 23
  ([#3801](https://github.com/haskell/haskell-language-server/pull/3801)) by @dependabot[bot]
- Add support for Fourmolu 0.14.0.0
  ([#3796](https://github.com/haskell/haskell-language-server/pull/3796)) by @brandonchinn178
- Add code lens  and fix code actions experiments
  ([#3791](https://github.com/haskell/haskell-language-server/pull/3791)) by @joyfulmantis
- Bump lsp versions in flake
  ([#3790](https://github.com/haskell/haskell-language-server/pull/3790)) by @colonelpanic8
- Clean up Release CI
  ([#3787](https://github.com/haskell/haskell-language-server/pull/3787)) by @fendor
- Do not suggest bogus module names
  ([#3784](https://github.com/haskell/haskell-language-server/pull/3784)) by @Bodigrim
- Delete `hls-haddock-comments-plugin`, `hls-stan-plugin`, and `hls-tactics-plugin`
  ([#3782](https://github.com/haskell/haskell-language-server/pull/3782)) by @michaelpj
- Enhance/releasing checklist
  ([#3781](https://github.com/haskell/haskell-language-server/pull/3781)) by @fendor
- Add cradle dependencies to session loading errors
  ([#3779](https://github.com/haskell/haskell-language-server/pull/3779)) by @VeryMilkyJoe
- Prepare release 2.2.0.0
  ([#3775](https://github.com/haskell/haskell-language-server/pull/3775)) by @fendor
- Add support for external Ormolu
  ([#3771](https://github.com/haskell/haskell-language-server/pull/3771)) by @sir4ur0n
- Support for resolve for class-plugin lenses
  ([#3769](https://github.com/haskell/haskell-language-server/pull/3769)) by @joyfulmantis
- Introduce declarative test project definition for plugin tests
  ([#3767](https://github.com/haskell/haskell-language-server/pull/3767)) by @fendor
- Use latest version of fourmolu possible
  ([#3764](https://github.com/haskell/haskell-language-server/pull/3764)) by @brandonchinn178
- Drop support for GHC 8.10
  ([#3434](https://github.com/haskell/haskell-language-server/pull/3434)) by @michaelpj


## 2.2.0.0

* Binaries for GHC 9.4.7
* Forward compatibility with latest VSCode client changes

### Pull Requests

-  hls-cabal-fmt-plugin: Use the file contents of the LSP request
  ([#3776](https://github.com/haskell/haskell-language-server/pull/3776)) by @fendor
- Adapt to lsp changes for workspace/configuration
  ([#3773](https://github.com/haskell/haskell-language-server/pull/3773)) by @michaelpj
- Rework "Configuration" and "Manually testing HLS" documentations
  ([#3772](https://github.com/haskell/haskell-language-server/pull/3772)) by @sir4ur0n
- Fix `main-is` completion suggestions not being relative to `hs-source-dirs`
  ([#3766](https://github.com/haskell/haskell-language-server/pull/3766)) by @VeryMilkyJoe
- Remove suggestion of stanzas inside of stanza context
  ([#3761](https://github.com/haskell/haskell-language-server/pull/3761)) by @VeryMilkyJoe
- Pedantic ghcide
  ([#3751](https://github.com/haskell/haskell-language-server/pull/3751)) by @joyfulmantis
- Fix #3574 and support resolve in explicit records
  ([#3750](https://github.com/haskell/haskell-language-server/pull/3750)) by @joyfulmantis

## 2.1.0.0

* Binaries for GHC 9.4.6
* Completions for .cabal files
* Performance improvements
* Show package name and its version while hovering on import statements
  ([#3691](https://github.com/haskell/haskell-language-server/pull/3691))
* Fix code edits in lsp spec compliant editors like helix.
  ([#3643](https://github.com/haskell/haskell-language-server/pull/3643))

### Pull requests merged

- Update to latest lsp packages
  ([#3747](https://github.com/haskell/haskell-language-server/pull/3747)) by @joyfulmantis
- Remove unnecessary allow-newer in stack.yaml
  ([#3746](https://github.com/haskell/haskell-language-server/pull/3746)) by @July541
- Log fourmolu and ormolu version that hls using
  ([#3744](https://github.com/haskell/haskell-language-server/pull/3744)) by @July541
- Various PluginError PR suggestions I missed earlier
  ([#3737](https://github.com/haskell/haskell-language-server/pull/3737)) by @joyfulmantis
- Add resolve support in refine imports by merging it with explicit imports
  ([#3729](https://github.com/haskell/haskell-language-server/pull/3729)) by @joyfulmantis
- Fix other file goto definition
  ([#3725](https://github.com/haskell/haskell-language-server/pull/3725)) by @nlander
- Fix Nix builds
  ([#3724](https://github.com/haskell/haskell-language-server/pull/3724)) by @cydparser
- Better plugin error infrastructure
  ([#3717](https://github.com/haskell/haskell-language-server/pull/3717)) by @joyfulmantis
- Move Recorder to hls-plugin-api
  ([#3714](https://github.com/haskell/haskell-language-server/pull/3714)) by @joyfulmantis
- Actually force usages
  ([#3713](https://github.com/haskell/haskell-language-server/pull/3713)) by @wz1000
- Best-effort support of Qualified Imports in GHC 9.4
  ([#3712](https://github.com/haskell/haskell-language-server/pull/3712)) by @konn
- Skip test if only CODEOWNERS changed
  ([#3707](https://github.com/haskell/haskell-language-server/pull/3707)) by @July541
- Update stack stuff
  ([#3706](https://github.com/haskell/haskell-language-server/pull/3706)) by @July541
- Mark hls-floskell-plugin as tier 3
  ([#3705](https://github.com/haskell/haskell-language-server/pull/3705)) by @July541
- Remove isovector as an owner
  ([#3700](https://github.com/haskell/haskell-language-server/pull/3700)) by @isovector
- Bump haskell/actions from 2.4.3 to 2.4.4 in /.github/actions/setup-build
  ([#3699](https://github.com/haskell/haskell-language-server/pull/3699)) by @dependabot[bot]
- Bump haskell/actions from 2.4.3 to 2.4.4
  ([#3698](https://github.com/haskell/haskell-language-server/pull/3698)) by @dependabot[bot]
- Catch exceptions in commands and use lsp null
  ([#3696](https://github.com/haskell/haskell-language-server/pull/3696)) by @joyfulmantis
- Show package name and its version while hovering on import statements
  ([#3691](https://github.com/haskell/haskell-language-server/pull/3691)) by @July541
- Resolve refactoring
  ([#3688](https://github.com/haskell/haskell-language-server/pull/3688)) by @joyfulmantis
- Prefer non-boot files when creating the FinderCache.
  ([#3687](https://github.com/haskell/haskell-language-server/pull/3687)) by @wz1000
- Some fixes for multi component stuff
  ([#3686](https://github.com/haskell/haskell-language-server/pull/3686)) by @wz1000
- Further hlint resolve changes.
  ([#3685](https://github.com/haskell/haskell-language-server/pull/3685)) by @joyfulmantis
- docs (plugin-support): fix plugin name typo
  ([#3683](https://github.com/haskell/haskell-language-server/pull/3683)) by @PiDelport
- Resolve for explicit-imports
  ([#3682](https://github.com/haskell/haskell-language-server/pull/3682)) by @joyfulmantis
- Hls 2.0.0.1 forward port
  ([#3680](https://github.com/haskell/haskell-language-server/pull/3680)) by @hasufell
- Resolve 2: Support for resolve in hls-hlint-plugin
  ([#3679](https://github.com/haskell/haskell-language-server/pull/3679)) by @joyfulmantis
- Resolve 0: Generic support for resolve in hls packages
  ([#3678](https://github.com/haskell/haskell-language-server/pull/3678)) by @joyfulmantis
- Ship hls-hlint-plugin for ghc-9.6
  ([#3677](https://github.com/haskell/haskell-language-server/pull/3677)) by @July541
- Remove extra call to newHscEnvEqWithImportPaths
  ([#3676](https://github.com/haskell/haskell-language-server/pull/3676)) by @nlander
- Fixes pragma plugin offering incorrect code actions #3673
  ([#3674](https://github.com/haskell/haskell-language-server/pull/3674)) by @joyfulmantis
- Restore short option for logfile
  ([#3672](https://github.com/haskell/haskell-language-server/pull/3672)) by @michaelpj
- Enable stylish-haskell for 9.6
  ([#3670](https://github.com/haskell/haskell-language-server/pull/3670)) by @michaelpj
- Bump supported ormolu, allow for 9.6
  ([#3668](https://github.com/haskell/haskell-language-server/pull/3668)) by @michaelpj
- Bump cachix/install-nix-action from 21 to 22
  ([#3666](https://github.com/haskell/haskell-language-server/pull/3666)) by @dependabot[bot]
- Add arguments to direct logs to various locations
  ([#3665](https://github.com/haskell/haskell-language-server/pull/3665)) by @michaelpj
- Support fourmolu 0.13
  ([#3662](https://github.com/haskell/haskell-language-server/pull/3662)) by @brandonchinn178
- Resolve 1: Support for resolve in overloaded-record-dot
  ([#3658](https://github.com/haskell/haskell-language-server/pull/3658)) by @joyfulmantis
- fix ISO8601 related deprecation in time
  ([#3654](https://github.com/haskell/haskell-language-server/pull/3654)) by @HugoPeters1024
- Add a log-level argument to set the log level
  ([#3651](https://github.com/haskell/haskell-language-server/pull/3651)) by @michaelpj
- Update Contributing.md
  ([#3650](https://github.com/haskell/haskell-language-server/pull/3650)) by @VeryMilkyJoe
- Commit to prettyprinter >= 1.7
  ([#3649](https://github.com/haskell/haskell-language-server/pull/3649)) by @michaelpj
- Add missing Monad constraint in the eval plugin
  ([#3648](https://github.com/haskell/haskell-language-server/pull/3648)) by @sandydoo
- hls-pragmas-plugin: Reduce noisy completions
  ([#3647](https://github.com/haskell/haskell-language-server/pull/3647)) by @akshaymankar
- Correctly pass VersionedTextDocumentIdentifier through hls
  ([#3643](https://github.com/haskell/haskell-language-server/pull/3643)) by @maralorn
- Add an assist for importing record fields when using OverloadedRecordDot
  ([#3642](https://github.com/haskell/haskell-language-server/pull/3642)) by @simmsb
- update flakes to compile with ghc928 and ghc962
  ([#3641](https://github.com/haskell/haskell-language-server/pull/3641)) by @smunix
- Split pragmas plugin by providers + decrease disable-warning priority
  ([#3640](https://github.com/haskell/haskell-language-server/pull/3640)) by @mrcjkb
- Reintroduce cabal-install in flake.nix
  ([#3637](https://github.com/haskell/haskell-language-server/pull/3637)) by @cgeorgii
- Delete dead cbits
  ([#3635](https://github.com/haskell/haskell-language-server/pull/3635)) by @michaelpj
- Simplify selection of GHCs to build on
  ([#3633](https://github.com/haskell/haskell-language-server/pull/3633)) by @michaelpj
- Support fourmolu 0.13.0.0
  ([#3631](https://github.com/haskell/haskell-language-server/pull/3631)) by @brandonchinn178
- Bump haskell/actions from 2.4.1 to 2.4.3 in /.github/actions/setup-build
  ([#3627](https://github.com/haskell/haskell-language-server/pull/3627)) by @dependabot[bot]
- Bump haskell/actions from 2.4.1 to 2.4.3
  ([#3626](https://github.com/haskell/haskell-language-server/pull/3626)) by @dependabot[bot]
- remove ghc minor versions in nix flake package builds
  ([#3625](https://github.com/haskell/haskell-language-server/pull/3625)) by @smunix
- HLS for the new generated LSP 2 release
  ([#3621](https://github.com/haskell/haskell-language-server/pull/3621)) by @joyfulmantis
- Keep plugin id of cabal-fmt in sync with default config id
  ([#3615](https://github.com/haskell/haskell-language-server/pull/3615)) by @VeryMilkyJoe
- Fix some grammar mistakes and typos
  ([#3614](https://github.com/haskell/haskell-language-server/pull/3614)) by @VeryMilkyJoe
- Bump cachix/install-nix-action from 20 to 21
  ([#3612](https://github.com/haskell/haskell-language-server/pull/3612)) by @dependabot[bot]
- fix: remove the `Indexing` progress message when exeption in withHieDb
  ([#3610](https://github.com/haskell/haskell-language-server/pull/3610)) by @guibou
- Bump haskell/actions from 2.4.0 to 2.4.1 in /.github/actions/setup-build
  ([#3604](https://github.com/haskell/haskell-language-server/pull/3604)) by @dependabot[bot]
- Bump haskell/actions from 2.4.0 to 2.4.1
  ([#3603](https://github.com/haskell/haskell-language-server/pull/3603)) by @dependabot[bot]
- Cabal file completions
  ([#3268](https://github.com/haskell/haskell-language-server/pull/3268)) by @VeryMilkyJoe
- Share ModuleGraphs for all files
  ([#3232](https://github.com/haskell/haskell-language-server/pull/3232)) by @wz1000

## 2.0.0.1

- Add overloaded record dot plugin initial version (closes #3350) (#3560)
- Binaries for GHC 9.2.8 and GHC 9.6.2

## 2.0.0.0

- New versioning scheme for all packages distributed as part of HLS,
  versioning them in lockstep for each release.
- Binaries for GHC 9.4.5
- Keep instance lenses stable even if parsed results are unavailable (#3545)
- Keep stale lenses for module name (#3570)
- Keep type lenses stable (#3558)

## 1.10.0.0

- Support for GHC 9.6
- Binaries for GHC 9.2.7 and GHC 9.6.1
- Eval plugin support for GHC 9.4+ (#3391)
- Don't show lenses for TH generated instances when using hls-class-plugin (#3531)

### Pull requests merged for 1.10.0.0
- Support fourmolu 0.11
([#3533](https://github.com/haskell/haskell-language-server/pull/3533)) by @brandonchinn178
- Don't show lenses for TH generated instances
([#3531](https://github.com/haskell/haskell-language-server/pull/3531)) by @July541
- Bump haskell/actions from 2.3.3 to 2.3.6
([#3529](https://github.com/haskell/haskell-language-server/pull/3529)) by @dependabot[bot]
- Use GHC 9.2.7 in flake
([#3527](https://github.com/haskell/haskell-language-server/pull/3527)) by @cydparser
- Remove HsLogger
([#3526](https://github.com/haskell/haskell-language-server/pull/3526)) by @fendor
- Use hie-bios 0.12
([#3524](https://github.com/haskell/haskell-language-server/pull/3524)) by @wz1000
- Bump haskell/actions
([#3520](https://github.com/haskell/haskell-language-server/pull/3520)) by @michaelpj
- Bump cachix/install-nix-action from 19 to 20
([#3514](https://github.com/haskell/haskell-language-server/pull/3514)) by @dependabot[bot]
- Docs: update Emacs section: add eglot with config example
([#3509](https://github.com/haskell/haskell-language-server/pull/3509)) by @m4lvin
- Eval plugin is now supported in 9.4
([#3508](https://github.com/haskell/haskell-language-server/pull/3508)) by @michaelpj
-  Update flake to GHC 9.2.6 and 9.4.4
([#3503](https://github.com/haskell/haskell-language-server/pull/3503)) by @cydparser
- Fix lower ghcide bounds of rename and fourmolu plugins
([#3501](https://github.com/haskell/haskell-language-server/pull/3501)) by @pepeiborra
- Add 9.2.6 to ghc-version-support.md
([#3494](https://github.com/haskell/haskell-language-server/pull/3494)) by @wz1000
- Bump versions and add changelogs for 1.9.1.0
([#3493](https://github.com/haskell/haskell-language-server/pull/3493)) by @hasufell
- Bump cachix/install-nix-action from 18 to 19
([#3490](https://github.com/haskell/haskell-language-server/pull/3490)) by @dependabot[bot]
- Experiment with loading matrix values from a file
([#3481](https://github.com/haskell/haskell-language-server/pull/3481)) by @michaelpj
- 9.6 support for HLS
([#3480](https://github.com/haskell/haskell-language-server/pull/3480)) by @wz1000
- Make the Ormolu plugin respect `.ormolu` fixity files when Ormolu ≥0.5.3.0
([#3449](https://github.com/haskell/haskell-language-server/pull/3449)) by @amesgen
- Migrate release CI to github
([#3406](https://github.com/haskell/haskell-language-server/pull/3406)) by @hasufell
- Eval plugin for GHC 9.4
([#3391](https://github.com/haskell/haskell-language-server/pull/3391)) by @wz1000

## 1.9.1.0

- Binaries for GHC 9.2.6.
- Fix for `hls-hlint-plugin` being unable to apply fixes due to GHC libdir from CI machines (#3241)
- Improvements for recompilation avoidance, particularly for non-vscode editors
  which don't support the LSP `workspace/didChangeWatchedFiles` method (#3458)

## 1.9.0.0

- Binaries for GHC 9.4.3, GHC 9.4.4 and GHC 9.2.5.
- Dropped support for GHC 8.8 and GHC 8.10.
- New plugins including:
  - Expanding record wild cards using hls-explicit-record-fields-plugin (#3304).
  - Formatting cabal fields using cabal-fmt via hls-cabal-fmt-plugin (#2047).
  - Warnings and errors for cabal files using hls-cabal-plugin (#2954).
  - Folding ranges using hls-code-range-plugin (#3058).
- Support for many plugins like the refactor, splice, retrie, gadt, hlint, fourmolu and class plugins.
- Completion for record dot fields (#3080).
- Performance and memory usage improvements.
- And many more bug fixes and improvements!

- Enable plugin tests in CI for GHC 9.4
([#3420](https://github.com/haskell/haskell-language-server/pull/3420)) by @wz1000
- Add a mergify action to update PRs
([#3418](https://github.com/haskell/haskell-language-server/pull/3418)) by @michaelpj
- GHC 9.4: Compute the source hash before the preprocessor
([#3415](https://github.com/haskell/haskell-language-server/pull/3415)) by @wz1000
- Clear the mi_globals field when generating an iface
([#3414](https://github.com/haskell/haskell-language-server/pull/3414)) by @wz1000
- Various strictness improvements
([#3413](https://github.com/haskell/haskell-language-server/pull/3413)) by @wz1000
- Remove unused GHC compat code
([#3412](https://github.com/haskell/haskell-language-server/pull/3412)) by @fendor
- Bump shake-bench to v0.2.0.0
([#3411](https://github.com/haskell/haskell-language-server/pull/3411)) by @pepeiborra
- Support fourmolu 0.10
([#3410](https://github.com/haskell/haskell-language-server/pull/3410)) by @brandonchinn178
- Fix nix build CI
([#3404](https://github.com/haskell/haskell-language-server/pull/3404)) by @wavewave
- Fix fourmolu with -f-fixity-th in nix env
([#3400](https://github.com/haskell/haskell-language-server/pull/3400)) by @wavewave
- Correct list of GHC versions in caching.yml to match test.yml
([#3397](https://github.com/haskell/haskell-language-server/pull/3397)) by @fendor
- Add CI flows for 9.4.3
([#3396](https://github.com/haskell/haskell-language-server/pull/3396)) by @pepeiborra
- Bump technote-space/get-diff-action from 6.1.1 to 6.1.2
([#3392](https://github.com/haskell/haskell-language-server/pull/3392)) by @dependabot[bot]
- Unload once per linkable instead of once per splice
([#3390](https://github.com/haskell/haskell-language-server/pull/3390)) by @wz1000
- Fix table in ghc-version-support.md
([#3389](https://github.com/haskell/haskell-language-server/pull/3389)) by @k4z4n0v4
- Fix Nix CI, probably
([#3388](https://github.com/haskell/haskell-language-server/pull/3388)) by @lf-
- [hls-explicit-record-fields-plugin] Expand used fields only
([#3386](https://github.com/haskell/haskell-language-server/pull/3386)) by @ozkutuk
- Bump hlint version CI flow
([#3384](https://github.com/haskell/haskell-language-server/pull/3384)) by @fendor
- Bump fkirc/skip-duplicate-actions from 5.2.0 to 5.3.0
([#3381](https://github.com/haskell/haskell-language-server/pull/3381)) by @dependabot[bot]
- Reword intro section in releases.md
([#3378](https://github.com/haskell/haskell-language-server/pull/3378)) by @fendor
- Make redundant import removal work on PatSyn imports
([#3377](https://github.com/haskell/haskell-language-server/pull/3377)) by @ozkutuk
- Add CI flows for GHC 9.2.5
([#3376](https://github.com/haskell/haskell-language-server/pull/3376)) by @fendor
- Delete dead code in hls-test-utils
([#3368](https://github.com/haskell/haskell-language-server/pull/3368)) by @fendor
- Bump gha versions in setup-build/action.yml
([#3366](https://github.com/haskell/haskell-language-server/pull/3366)) by @fendor
- Wingman copy old to new
([#3363](https://github.com/haskell/haskell-language-server/pull/3363)) by @santiweight
- Cleanup Development.IDE.CodeAction
([#3360](https://github.com/haskell/haskell-language-server/pull/3360)) by @santiweight
- Use latest GHC 9.2 and 9.4 in flake.nix
([#3354](https://github.com/haskell/haskell-language-server/pull/3354)) by @cydparser
- wingman: move wingman to new directory
([#3352](https://github.com/haskell/haskell-language-server/pull/3352)) by @santiweight
- Introduce common code for Recorders in Plugin Tests
([#3347](https://github.com/haskell/haskell-language-server/pull/3347)) by @fendor
- Add `RangeMap` for unified "in-range" filtering
([#3343](https://github.com/haskell/haskell-language-server/pull/3343)) by @ozkutuk
- Docs: update and split neovim/vim configurations
([#3342](https://github.com/haskell/haskell-language-server/pull/3342)) by @MrcJkb
- Extract AddArgument modules
([#3339](https://github.com/haskell/haskell-language-server/pull/3339)) by @santiweight
- Add hls-cabal-fmt-plugin to hackage release CI script and HLS library
([#3335](https://github.com/haskell/haskell-language-server/pull/3335)) by @fendor
- Ensure at least 1 capability
([#3334](https://github.com/haskell/haskell-language-server/pull/3334)) by @pepeiborra
- Add support for Fourmolu 0.9
([#3331](https://github.com/haskell/haskell-language-server/pull/3331)) by @brandonchinn178
- [skip ci] Add myself to CODEOWNERS
([#3329](https://github.com/haskell/haskell-language-server/pull/3329)) by @ozkutuk
- Typo fixes
([#3325](https://github.com/haskell/haskell-language-server/pull/3325)) by @Deltaspace0
- Gitlab CI improvements
([#3324](https://github.com/haskell/haskell-language-server/pull/3324)) by @wz1000
- Refactor overlay composition
([#3323](https://github.com/haskell/haskell-language-server/pull/3323)) by @Gabriella439
- Add support for `.env` shells to `flake.nix`
([#3322](https://github.com/haskell/haskell-language-server/pull/3322)) by @Gabriella439
- feat: update type signature during add argument action
([#3321](https://github.com/haskell/haskell-language-server/pull/3321)) by @santiweight
- Update refactor/splice/hlint/fourmolu/retrie/gadt plugin for GHC 9.4
([#3317](https://github.com/haskell/haskell-language-server/pull/3317)) by @9999years
- Remove stack from installation docs since it is not supported anymore
([#3314](https://github.com/haskell/haskell-language-server/pull/3314)) by @fendor
- Bump cachix/cachix-action from 11 to 12
([#3310](https://github.com/haskell/haskell-language-server/pull/3310)) by @dependabot[bot]
- Restore ability to run source plugins
([#3309](https://github.com/haskell/haskell-language-server/pull/3309)) by @JakobBruenker
- New plugin: Explicit record fields
([#3304](https://github.com/haskell/haskell-language-server/pull/3304)) by @ozkutuk
- support haddock-library 1.11
([#3303](https://github.com/haskell/haskell-language-server/pull/3303)) by @kokobd
- Record diagnostics source rule when testing
([#3301](https://github.com/haskell/haskell-language-server/pull/3301)) by @pepeiborra
- Make a test more reliable
([#3300](https://github.com/haskell/haskell-language-server/pull/3300)) by @pepeiborra
- Change default cabal install target name on docs/installation.md
([#3298](https://github.com/haskell/haskell-language-server/pull/3298)) by @caiquefigueiredo
- Bump technote-space/get-diff-action from 6.1.0 to 6.1.1
([#3293](https://github.com/haskell/haskell-language-server/pull/3293)) by @dependabot[bot]
- Bump cachix/install-nix-action from 17 to 18
([#3292](https://github.com/haskell/haskell-language-server/pull/3292)) by @dependabot[bot]
- Bump cachix/cachix-action from 10 to 11
([#3291](https://github.com/haskell/haskell-language-server/pull/3291)) by @dependabot[bot]
- Purge GHC 8.8
([#3287](https://github.com/haskell/haskell-language-server/pull/3287)) by @michaelpj
- Bump partial ghc support warning to 9.4
([#3286](https://github.com/haskell/haskell-language-server/pull/3286)) by @andys8
- Improved message for missing command or plugin
([#3285](https://github.com/haskell/haskell-language-server/pull/3285)) by @andys8
- Register Fourmolu plugin properties
([#3284](https://github.com/haskell/haskell-language-server/pull/3284)) by @georgefst
- Cleanup GHC macros (because min version is 8.8.4)
([#3281](https://github.com/haskell/haskell-language-server/pull/3281)) by @andys8
- Remove unlawful Ord instance and replace it by a compare function
([#3279](https://github.com/haskell/haskell-language-server/pull/3279)) by @ChristophHochrainer
- Exclude the implicit prelude import (#2798)
([#3277](https://github.com/haskell/haskell-language-server/pull/3277)) by @ChristophHochrainer
- Fix typos in documentation
([#3274](https://github.com/haskell/haskell-language-server/pull/3274)) by @bendo
- Use an importance score to order the suggested import code action
([#3271](https://github.com/haskell/haskell-language-server/pull/3271)) by @ChristophHochrainer
- Update plugin tutorial
([#3266](https://github.com/haskell/haskell-language-server/pull/3266)) by @dyniec
- configuration-ghc-94.nix: Fix references to lsp and lsp-types source
([#3265](https://github.com/haskell/haskell-language-server/pull/3265)) by @akshaymankar
- Add suggestions about licenses in cabal file
([#3261](https://github.com/haskell/haskell-language-server/pull/3261)) by @dyniec
- Fix action removes ticks from TemplateHaskellQuotes (#628)
([#3260](https://github.com/haskell/haskell-language-server/pull/3260)) by @bendo
- Hlint: A handful of fixes to hints
([#3259](https://github.com/haskell/haskell-language-server/pull/3259)) by @andys8
- Support ghc 9.4 for hls-class-plugin
([#3258](https://github.com/haskell/haskell-language-server/pull/3258)) by @July541
- Fix nix developement shell
([#3257](https://github.com/haskell/haskell-language-server/pull/3257)) by @akshaymankar
- GCH -> GHC
([#3252](https://github.com/haskell/haskell-language-server/pull/3252)) by @michaelpj
- Docs: Plugin Support hls-explicit-fixity-plugin
([#3251](https://github.com/haskell/haskell-language-server/pull/3251)) by @andys8
- Fix dead link to supported GHC versions
([#3244](https://github.com/haskell/haskell-language-server/pull/3244)) by @buggymcbugfix
- Update link to supported versions in README
([#3242](https://github.com/haskell/haskell-language-server/pull/3242)) by @citrusmunch
- Bump fkirc/skip-duplicate-actions from 5.1.0 to 5.2.0
([#3239](https://github.com/haskell/haskell-language-server/pull/3239)) by @dependabot[bot]
- Move new imports down the code action list
([#3235](https://github.com/haskell/haskell-language-server/pull/3235)) by @kokobd
- Improve memory characteristics of ExportsMap
([#3231](https://github.com/haskell/haskell-language-server/pull/3231)) by @wz1000
- [skip ci] Remove myself from codeowners
([#3230](https://github.com/haskell/haskell-language-server/pull/3230)) by @jneira
- Fix error in code range
([#3229](https://github.com/haskell/haskell-language-server/pull/3229)) by @kokobd
- Use nixpkgs variants of Sphinx packages
([#3227](https://github.com/haskell/haskell-language-server/pull/3227)) by @ozkutuk
- Bump fkirc/skip-duplicate-actions from 4.0.0 to 5.1.0
([#3226](https://github.com/haskell/haskell-language-server/pull/3226)) by @dependabot[bot]
- Add `source-repository` to all cabal files
([#3219](https://github.com/haskell/haskell-language-server/pull/3219)) by @hololeap
- hls-hlint-plugin: Update README.md
([#3216](https://github.com/haskell/haskell-language-server/pull/3216)) by @hololeap
-  wrapper.in: Require runtime ghc-pkgs to be an abi compatible superset of bootpkgs
([#3214](https://github.com/haskell/haskell-language-server/pull/3214)) by @maralorn
- Add diagnostics to Stan descriptor
([#3213](https://github.com/haskell/haskell-language-server/pull/3213)) by @pepeiborra
- Document the `stack` requirement in wrapper tests
([#3212](https://github.com/haskell/haskell-language-server/pull/3212)) by @ozkutuk
- Improve haddock comments
([#3207](https://github.com/haskell/haskell-language-server/pull/3207)) by @kokobd
- Implement sharing for hls-graph Keys
([#3206](https://github.com/haskell/haskell-language-server/pull/3206)) by @wz1000
- Improve hls-fixity-plugin
([#3205](https://github.com/haskell/haskell-language-server/pull/3205)) by @wz1000
- Implement completionItem/resolve
([#3204](https://github.com/haskell/haskell-language-server/pull/3204)) by @wz1000
- Sort vscode extension schema json by keys
([#3203](https://github.com/haskell/haskell-language-server/pull/3203)) by @fendor
- docs/supported-versions: Fix typo and more precise brittany support
([#3201](https://github.com/haskell/haskell-language-server/pull/3201)) by @maralorn
- Stylish Haskell: CPP parse issues
([#3199](https://github.com/haskell/haskell-language-server/pull/3199)) by @andys8
- Bump technote-space/get-diff-action from 4.0.1 to 6.1.0
([#3198](https://github.com/haskell/haskell-language-server/pull/3198)) by @dependabot[bot]
- Log plugin name and attribute errors to plugins
([#3194](https://github.com/haskell/haskell-language-server/pull/3194)) by @pepeiborra
- Support optional plugins
([#3193](https://github.com/haskell/haskell-language-server/pull/3193)) by @pepeiborra
- Add policy on plugin support tiers
([#3189](https://github.com/haskell/haskell-language-server/pull/3189)) by @michaelpj
- Fix broken call-hierarchy-plugin-tests for type signatures
([#3188](https://github.com/haskell/haskell-language-server/pull/3188)) by @July541
- Update supported GHC versions doc
([#3186](https://github.com/haskell/haskell-language-server/pull/3186)) by @michaelpj
- Docs: Fix checkParents documentation
([#3184](https://github.com/haskell/haskell-language-server/pull/3184)) by @andys8
- Configuration: more advanced Vim / Coc example (suggestion)
([#3181](https://github.com/haskell/haskell-language-server/pull/3181)) by @andys8
- Docs: List stan plugin
([#3180](https://github.com/haskell/haskell-language-server/pull/3180)) by @andys8
- Stan: Respect plugin configuration globalOn
([#3179](https://github.com/haskell/haskell-language-server/pull/3179)) by @andys8
- Solve formatting issues (stylish-haskell, pre-commit CI)
([#3171](https://github.com/haskell/haskell-language-server/pull/3171)) by @andys8
- remove manual heap profiling from ghcide
([#3168](https://github.com/haskell/haskell-language-server/pull/3168)) by @pepeiborra
- Refactor plugin: Prefer code action
([#3167](https://github.com/haskell/haskell-language-server/pull/3167)) by @andys8
- Fixes the flake deps to align with cabal bounds
([#3163](https://github.com/haskell/haskell-language-server/pull/3163)) by @mjrussell
- Remove unused build-depends and install warnings
([#3155](https://github.com/haskell/haskell-language-server/pull/3155)) by @pepeiborra
- Release script fixes
([#3154](https://github.com/haskell/haskell-language-server/pull/3154)) by @wz1000
- Allow hackage upload
([#3153](https://github.com/haskell/haskell-language-server/pull/3153)) by @wz1000
- support add-argument action
([#3149](https://github.com/haskell/haskell-language-server/pull/3149)) by @santiweight
- Only run the pre-commit hook on changed files
([#3145](https://github.com/haskell/haskell-language-server/pull/3145)) by @drsooch
- unescape printable characters
([#3140](https://github.com/haskell/haskell-language-server/pull/3140)) by @kokobd
- nix: fix nix environment for GHC 9.4
([#3133](https://github.com/haskell/haskell-language-server/pull/3133)) by @guibou
- Drop compatibility with GHC 8.6.5
([#3101](https://github.com/haskell/haskell-language-server/pull/3101)) by @pepeiborra
- Feat: basic record dot completions
([#3080](https://github.com/haskell/haskell-language-server/pull/3080)) by @coltenwebb
- Feat: Folding Ranges
([#3058](https://github.com/haskell/haskell-language-server/pull/3058)) by @sloorush
- Parse .cabal files; show error and warning diagnostics
([#2954](https://github.com/haskell/haskell-language-server/pull/2954)) by @runeksvendsen
- Make splice plugin compatible with GHC 9.2
([#2816](https://github.com/haskell/haskell-language-server/pull/2816)) by @eddiejessup
- Add formatting plugin for cabal files which uses cabal-fmt
([#2047](https://github.com/haskell/haskell-language-server/pull/2047)) by @VeryMilkyJoe

## 1.8.0.0

- Binaries for GHC 9.2.3 and GHC 9.2.4
- Initial support for GHC 9.4 with binaries for GHC 9.4.1 and GHC 9.4.2
- Startup time and performance improvements on projects using Template Haskell by serializing intermediate core (#2813)
- Memory usage improvements due to using a packed representation for filepaths (#3067, @kokobd)
- Improvements for hls-class-plugin (#2920, @July541)
- The new hls-gadt-plugin (#2899, @July541)
- Moving code actions from ghcide to the new hls-refactor-plugin (#3091, @wz1000)
- Many more improvements and bug fixes thanks to our contributors!

### Pull requests merged for 1.8.0.0

- Alternate Number Format Plugin buildable with GHC 9.4
([#3138](https://github.com/haskell/haskell-language-server/pull/3138)) by @drsooch
- Enable a bunch of plugins that build with ghc 9.4
([#3136](https://github.com/haskell/haskell-language-server/pull/3136)) by @pepeiborra
- Enable support for 9.4 on windows
([#3132](https://github.com/haskell/haskell-language-server/pull/3132)) by @wz1000
- flake.nix Add ghcide-bench to sourceDirs
([#3125](https://github.com/haskell/haskell-language-server/pull/3125)) by @akshaymankar
- Update hls-retrie-plugin to be usable with 9.2.4.
([#3120](https://github.com/haskell/haskell-language-server/pull/3120)) by @drsooch
- Add link to homepage and issues for `hie-compat`
([#3119](https://github.com/haskell/haskell-language-server/pull/3119)) by @parsonsmatt
- Remove pluginId from getNormalizedFilePath error message
([#3118](https://github.com/haskell/haskell-language-server/pull/3118)) by @drsooch
- HLS benchmarks
([#3117](https://github.com/haskell/haskell-language-server/pull/3117)) by @pepeiborra
- Fix --testing
([#3113](https://github.com/haskell/haskell-language-server/pull/3113)) by @pepeiborra
- Deduplicate HLS plugins
([#3112](https://github.com/haskell/haskell-language-server/pull/3112)) by @pepeiborra
- Do not send Heap Stats to the LSP log
([#3111](https://github.com/haskell/haskell-language-server/pull/3111)) by @pepeiborra
- Send begin progress message synchronously
([#3110](https://github.com/haskell/haskell-language-server/pull/3110)) by @pepeiborra
- Remove unused config in hls-class-plugin
([#3107](https://github.com/haskell/haskell-language-server/pull/3107)) by @July541
- Support fourmolu-0.8.1.0
([#3103](https://github.com/haskell/haskell-language-server/pull/3103)) by @brandonchinn178
- Probe-tools: Print stack ghc version
([#3093](https://github.com/haskell/haskell-language-server/pull/3093)) by @andys8
- Fix #3047
([#3092](https://github.com/haskell/haskell-language-server/pull/3092)) by @July541
- Remove exactprint dependencies from ghcide by introducing hls-refactor-plugin.
([#3091](https://github.com/haskell/haskell-language-server/pull/3091)) by @wz1000
- Stan: Avoid terminal colors in messages
([#3090](https://github.com/haskell/haskell-language-server/pull/3090)) by @andys8
- Support ghc-9.2.4
([#3085](https://github.com/haskell/haskell-language-server/pull/3085)) by @July541
- Bump Nix flake GHC 9.2.3 to 9.2.4
([#3081](https://github.com/haskell/haskell-language-server/pull/3081)) by @cydparser
- fix lsp-types benchmark
([#3079](https://github.com/haskell/haskell-language-server/pull/3079)) by @pepeiborra
- Add support for Fourmolu 0.8
([#3078](https://github.com/haskell/haskell-language-server/pull/3078)) by @brandonchinn178
- upgrade lsp to 1.5
([#3072](https://github.com/haskell/haskell-language-server/pull/3072)) by @kokobd
- Bump actions/cache from 2 to 3
([#3071](https://github.com/haskell/haskell-language-server/pull/3071)) by @dependabot[bot]
- Bump actions/setup-python from 3 to 4
([#3070](https://github.com/haskell/haskell-language-server/pull/3070)) by @dependabot[bot]
- Run the benchmark suite on GHC 9.2.3
([#3069](https://github.com/haskell/haskell-language-server/pull/3069)) by @pepeiborra
- Simplify instructions about 'ghcup compile hls'
([#3068](https://github.com/haskell/haskell-language-server/pull/3068)) by @hasufell
- Improve performance of NormalizedFilePath
([#3067](https://github.com/haskell/haskell-language-server/pull/3067)) by @kokobd
- add a prefix to plugin CPP definitions
([#3065](https://github.com/haskell/haskell-language-server/pull/3065)) by @kokobd
- Add Github precommit workflow
([#3060](https://github.com/haskell/haskell-language-server/pull/3060)) by @lunaticare
- Run pre-commit hooks
([#3059](https://github.com/haskell/haskell-language-server/pull/3059)) by @lunaticare
- Fix grammar and spelling errors in configuration.md
([#3056](https://github.com/haskell/haskell-language-server/pull/3056)) by @arsenkhy
- Remove redundant WARNING prefix
([#3055](https://github.com/haskell/haskell-language-server/pull/3055)) by @michaelpj
- fix a typo in src/Ide/Plugin/Class/CodeLens.hs
([#3053](https://github.com/haskell/haskell-language-server/pull/3053)) by @tensorknower69
- fix record-dot-syntax test
([#3051](https://github.com/haskell/haskell-language-server/pull/3051)) by @coltenwebb
- build(nix): ghc922 -> ghc923
([#3049](https://github.com/haskell/haskell-language-server/pull/3049)) by @teto
- build(nix): bumped gitignore dependency
([#3048](https://github.com/haskell/haskell-language-server/pull/3048)) by @teto
- Update issue templates
([#3044](https://github.com/haskell/haskell-language-server/pull/3044)) by @michaelpj
- Simplify hlint config
([#3038](https://github.com/haskell/haskell-language-server/pull/3038)) by @michaelpj
- handle trailing comma in import list properly
([#3035](https://github.com/haskell/haskell-language-server/pull/3035)) by @kokobd
- upgrade ghc-check to fix #3002
([#3034](https://github.com/haskell/haskell-language-server/pull/3034)) by @kokobd
- Fix Stack build with Nix on macOS
([#3031](https://github.com/haskell/haskell-language-server/pull/3031)) by @lunaticare
- haskell-language-server: add lower bound for githash
([#3030](https://github.com/haskell/haskell-language-server/pull/3030)) by @Bodigrim
- hls-eval-plugin: add lower bound for parser-combinators
([#3029](https://github.com/haskell/haskell-language-server/pull/3029)) by @Bodigrim
- hls-fourmolu-plugin: add lower bound for process-extras
([#3028](https://github.com/haskell/haskell-language-server/pull/3028)) by @Bodigrim
- ghcide: lower bounds
([#3025](https://github.com/haskell/haskell-language-server/pull/3025)) by @Bodigrim
- remove all usages of pre-commit-check in nix
([#3024](https://github.com/haskell/haskell-language-server/pull/3024)) by @kokobd
- hls-plugin-api: add lower bounds
([#3022](https://github.com/haskell/haskell-language-server/pull/3022)) by @Bodigrim
- hls-graph: add lower bound for async
([#3021](https://github.com/haskell/haskell-language-server/pull/3021)) by @Bodigrim
- Hlint: CodeAction with isPreferred
([#3018](https://github.com/haskell/haskell-language-server/pull/3018)) by @andys8
- Record Dot Hover Types
([#3016](https://github.com/haskell/haskell-language-server/pull/3016)) by @coltenwebb
- re-enable haddock
([#3015](https://github.com/haskell/haskell-language-server/pull/3015)) by @kokobd
- add Helix to configuration.md
([#3014](https://github.com/haskell/haskell-language-server/pull/3014)) by @0rphee
- Renaming of indirect references (RecordFieldPuns)
([#3013](https://github.com/haskell/haskell-language-server/pull/3013)) by @OliverMadine
- Revert back to Warning not Error in Logging `ResponseErrors`
([#3009](https://github.com/haskell/haskell-language-server/pull/3009)) by @drsooch
- Disable flaky test on Windows
([#3008](https://github.com/haskell/haskell-language-server/pull/3008)) by @michaelpj
- Improve troubleshooting and installation docs a bit
([#3004](https://github.com/haskell/haskell-language-server/pull/3004)) by @michaelpj
- refactor selection range plugin
([#3003](https://github.com/haskell/haskell-language-server/pull/3003)) by @kokobd
- Hlint more partial functions, and Debug.Trace
([#3000](https://github.com/haskell/haskell-language-server/pull/3000)) by @michaelpj
- Don't use typecheck rule for non FOIs in refine imports plugin
([#2995](https://github.com/haskell/haskell-language-server/pull/2995)) by @wz1000
- GHC 9.4 compatibility + Multiple Home Units
([#2994](https://github.com/haskell/haskell-language-server/pull/2994)) by @wz1000
- unify pre-commit hook & update Gitpod config
([#2991](https://github.com/haskell/haskell-language-server/pull/2991)) by @kokobd
- Log response errors returned from Plugins
([#2988](https://github.com/haskell/haskell-language-server/pull/2988)) by @drsooch
- Add associated type families to local completions
([#2987](https://github.com/haskell/haskell-language-server/pull/2987)) by @gasparattila
- Remove some partial functions from Shake.hs
([#2986](https://github.com/haskell/haskell-language-server/pull/2986)) by @michaelpj
- Clean up ghc-9.0 partial support contents
([#2983](https://github.com/haskell/haskell-language-server/pull/2983)) by @July541
- fix new import position
([#2981](https://github.com/haskell/haskell-language-server/pull/2981)) by @kokobd
- Implement PluginMethod for hard-wired in handlers
([#2977](https://github.com/haskell/haskell-language-server/pull/2977)) by @fendor
- Set up partial functions ratchet
([#2974](https://github.com/haskell/haskell-language-server/pull/2974)) by @michaelpj
- Turn HLS-wrapper into an LSP Server
([#2960](https://github.com/haskell/haskell-language-server/pull/2960)) by @smatting
- More Fourmolu improvements
([#2959](https://github.com/haskell/haskell-language-server/pull/2959)) by @georgefst
- hls-class-plugin: Only create placeholders for unimplemented methods
([#2956](https://github.com/haskell/haskell-language-server/pull/2956)) by @akshaymankar
- Fix Fourmolu 0.7 support
([#2950](https://github.com/haskell/haskell-language-server/pull/2950)) by @georgefst
- Teach HLS about different file extensions
([#2945](https://github.com/haskell/haskell-language-server/pull/2945)) by @fendor
- Support `fourmolu ^>= 0.7`
([#2944](https://github.com/haskell/haskell-language-server/pull/2944)) by @parsonsmatt
- hls-explicit-fixity-plugin
([#2941](https://github.com/haskell/haskell-language-server/pull/2941)) by @July541
- chore(nix): bump nixpkgs to prevent glibc issues
([#2937](https://github.com/haskell/haskell-language-server/pull/2937)) by @teto
- Support ghc-9.2.3
([#2936](https://github.com/haskell/haskell-language-server/pull/2936)) by @July541
- Typo fix, dependecies -> dependencies
([#2934](https://github.com/haskell/haskell-language-server/pull/2934)) by @vikrem
- Update Archlinux installation section
([#2933](https://github.com/haskell/haskell-language-server/pull/2933)) by @marcin-rzeznicki
- docs/installation: Remove unused clone with submodule command
([#2930](https://github.com/haskell/haskell-language-server/pull/2930)) by @sloorush
- Omit more parens for wildcard type signature
([#2929](https://github.com/haskell/haskell-language-server/pull/2929)) by @sergv
- Add `throwPluginError` to Plugin Utilities
([#2924](https://github.com/haskell/haskell-language-server/pull/2924)) by @drsooch
- hls-class-plugin enhancement
([#2920](https://github.com/haskell/haskell-language-server/pull/2920)) by @July541
- Bump documentation requirements
([#2918](https://github.com/haskell/haskell-language-server/pull/2918)) by @xsebek
- Document eval plugin limitations
([#2917](https://github.com/haskell/haskell-language-server/pull/2917)) by @xsebek
- Replace TextDocumentIdentifier with Uri in getNormalizedFilePath
([#2912](https://github.com/haskell/haskell-language-server/pull/2912)) by @July541
- Fix hover format
([#2911](https://github.com/haskell/haskell-language-server/pull/2911)) by @July541
- Fix multiline eval plugin padding
([#2910](https://github.com/haskell/haskell-language-server/pull/2910)) by @xsebek
- Stan integration #258
([#2908](https://github.com/haskell/haskell-language-server/pull/2908)) by @uhbif19
- A plugin for GADT syntax converter
([#2899](https://github.com/haskell/haskell-language-server/pull/2899)) by @July541
- Fix DisplayTHWarning error
([#2895](https://github.com/haskell/haskell-language-server/pull/2895)) by @pepeiborra
- Enable hls-eval-plugin test on ghc-9.2.2
([#2893](https://github.com/haskell/haskell-language-server/pull/2893)) by @July541
- nix update
([#2892](https://github.com/haskell/haskell-language-server/pull/2892)) by @michaelpj
- Build hls-alternate-number-format-plugin with stack.yaml
([#2891](https://github.com/haskell/haskell-language-server/pull/2891)) by @July541
- Modify ghcide requirements of hls-change-type-signature-plugin
([#2889](https://github.com/haskell/haskell-language-server/pull/2889)) by @July541
- Fix hls-call-hierarchy-plugin tests
([#2888](https://github.com/haskell/haskell-language-server/pull/2888)) by @July541
- Add .txt files as extra-source-files for hls-change-type-signature-plugin
([#2887](https://github.com/haskell/haskell-language-server/pull/2887)) by @cdepillabout
- Prefer Data.HashSet.member to Data.Foldable.elem
([#2886](https://github.com/haskell/haskell-language-server/pull/2886)) by @sergv
- no longer disable -dynamic in CI
([#2885](https://github.com/haskell/haskell-language-server/pull/2885)) by @pepeiborra
- hls-pragmas-plugin requires ghcide >= 1.7
([#2884](https://github.com/haskell/haskell-language-server/pull/2884)) by @Bodigrim
- Make iface-error-test-1 less flaky
([#2882](https://github.com/haskell/haskell-language-server/pull/2882)) by @pepeiborra
- hls-haddock-comments does not support ghc-exactprint >= 1.0
([#2878](https://github.com/haskell/haskell-language-server/pull/2878)) by @Bodigrim
- Restore compat. with prettyprinter 1.6
([#2877](https://github.com/haskell/haskell-language-server/pull/2877)) by @pepeiborra
- ghcide requires ghc-exactprint >= 1.4
([#2876](https://github.com/haskell/haskell-language-server/pull/2876)) by @Bodigrim
- ghcide needs prettyprinter-1.7 to build
([#2875](https://github.com/haskell/haskell-language-server/pull/2875)) by @juhp
- Review project stack descriptors according to #2533
([#2874](https://github.com/haskell/haskell-language-server/pull/2874)) by @pepeiborra
- hls-call-hierarchy-plugin Patch release
([#2873](https://github.com/haskell/haskell-language-server/pull/2873)) by @pepeiborra
- Expand input to pragma if available
([#2871](https://github.com/haskell/haskell-language-server/pull/2871)) by @July541
- Fix hanging redundant import on Unicode function
([#2870](https://github.com/haskell/haskell-language-server/pull/2870)) by @drsooch
- Compatibility with older aeson releases
([#2868](https://github.com/haskell/haskell-language-server/pull/2868)) by @pepeiborra
- simplify hlint plugin Cabal descriptor
([#2867](https://github.com/haskell/haskell-language-server/pull/2867)) by @pepeiborra
- Consolidate all cabal.project files
([#2866](https://github.com/haskell/haskell-language-server/pull/2866)) by @pepeiborra
- release script fixes
([#2861](https://github.com/haskell/haskell-language-server/pull/2861)) by @wz1000
- Support hls-hlint-plugin and hls-stylish-plugin for ghc9.0 and ghc9.2
([#2854](https://github.com/haskell/haskell-language-server/pull/2854)) by @July541
- Bump haskell/actions from 1 to 2
([#2852](https://github.com/haskell/haskell-language-server/pull/2852)) by @dependabot[bot]
- Add scripts for releases and final 1.7 tweaks
([#2850](https://github.com/haskell/haskell-language-server/pull/2850)) by @wz1000
- Fix Completion document format
([#2848](https://github.com/haskell/haskell-language-server/pull/2848)) by @July541
- Improve name export code action
([#2847](https://github.com/haskell/haskell-language-server/pull/2847)) by @sergv
- Update plugin support table
([#2840](https://github.com/haskell/haskell-language-server/pull/2840)) by @michaelpj
- Unify showSDocUnsafe
([#2830](https://github.com/haskell/haskell-language-server/pull/2830)) by @July541
- ghcide: remove redundant env NIX_GHC_LIBDIR
([#2819](https://github.com/haskell/haskell-language-server/pull/2819)) by @sloorush
- Serialize Core
([#2813](https://github.com/haskell/haskell-language-server/pull/2813)) by @wz1000
- Expose runtime metrics via EKG
([#2267](https://github.com/haskell/haskell-language-server/pull/2267)) by @pepeiborra

## 1.7.0.0

- Distribute dynamically linked binaries for HLS to avoid statically linking against GLIBC
  and system libraries, and to avoid unpredictable failures due to subtle differences
  between the GHC used to compile HLS and the GHC installed on the users machine
  (@hasufell, #2675, #2431)

- Improved recompilation avoidance in projects that make use of Template Haskell (#2316). See
  the [blog post](https://well-typed.com/blog/2022/04/hls-performance/) for more details.
  This release includes the `avoid-recompile` set of commits described in the blog post.

- Support for GHC 9.2.2

- Removal of HLS installer scripts as mentioned by the deprecation notice last release (#2773)

- Many more improvements and bug fixed thanks to our contributors!

### Pull requests merged for 1.6.1.1

- Restore concise type variables in ghc-9.2
([#2828](https://github.com/haskell/haskell-language-server/pull/2828)) by @July541
- Should no related code lens if the module name is correct
([#2826](https://github.com/haskell/haskell-language-server/pull/2826)) by @July541
- Bump cachix/install-nix-action from 16 to 17
([#2823](https://github.com/haskell/haskell-language-server/pull/2823)) by @dependabot[bot]
- Bump actions/upload-artifact from 2 to 3
([#2822](https://github.com/haskell/haskell-language-server/pull/2822)) by @dependabot[bot]
- Bump actions/download-artifact from 2 to 3
([#2821](https://github.com/haskell/haskell-language-server/pull/2821)) by @dependabot[bot]
- bench: Add more metrics
([#2814](https://github.com/haskell/haskell-language-server/pull/2814)) by @wz1000
- Enable rename plugin
([#2809](https://github.com/haskell/haskell-language-server/pull/2809)) by @OliverMadine
- Fix `cabal install` commands for local HLS build in docs
([#2807](https://github.com/haskell/haskell-language-server/pull/2807)) by @9999years
- Bump actions/cache from 2 to 3
([#2806](https://github.com/haskell/haskell-language-server/pull/2806)) by @dependabot[bot]
- [hls-graph] Optimise waitConcurrently
([#2805](https://github.com/haskell/haskell-language-server/pull/2805)) by @pepeiborra
- [bench] track changes to hls-* projects
([#2803](https://github.com/haskell/haskell-language-server/pull/2803)) by @pepeiborra
- Fix Show instance
([#2802](https://github.com/haskell/haskell-language-server/pull/2802)) by @pepeiborra
- Provide all format suggestions in AlternatFormat Code Action
([#2790](https://github.com/haskell/haskell-language-server/pull/2790)) by @drsooch
- Avoid race conditions with VFS and VFS versions
([#2789](https://github.com/haskell/haskell-language-server/pull/2789)) by @wz1000
- Don't show the redundant space
([#2788](https://github.com/haskell/haskell-language-server/pull/2788)) by @July541
- Target GHC 9.2.2
([#2787](https://github.com/haskell/haskell-language-server/pull/2787)) by @pepeiborra
- Allow import all constructors
([#2782](https://github.com/haskell/haskell-language-server/pull/2782)) by @July541
- Customizable TH warning
([#2781](https://github.com/haskell/haskell-language-server/pull/2781)) by @pepeiborra
- Fix #2693
([#2780](https://github.com/haskell/haskell-language-server/pull/2780)) by @wz1000
- Add Gentoo installation details
([#2778](https://github.com/haskell/haskell-language-server/pull/2778)) by @paul-jewell
- Eval plugin: mark exceptions
([#2775](https://github.com/haskell/haskell-language-server/pull/2775)) by @xsebek
- Fix 2 space leaks
([#2774](https://github.com/haskell/haskell-language-server/pull/2774)) by @pepeiborra
- Delete HLS installer scripts
([#2773](https://github.com/haskell/haskell-language-server/pull/2773)) by @fendor
- Purge some more hslogger
([#2770](https://github.com/haskell/haskell-language-server/pull/2770)) by @michaelpj
- Abbreviate explicit import code lenses
([#2769](https://github.com/haskell/haskell-language-server/pull/2769)) by @michaelpj
- Review masking and add traces when things don't cancel timely
([#2768](https://github.com/haskell/haskell-language-server/pull/2768)) by @pepeiborra
- Upgrade to hie-bios 0.9.1
([#2766](https://github.com/haskell/haskell-language-server/pull/2766)) by @fendor
- Avoid extra parens for wildcard type signature
([#2764](https://github.com/haskell/haskell-language-server/pull/2764)) by @xsebek
- Add an option to run Fourmolu via the CLI interface of a separate binary, rather than the bundled library
([#2763](https://github.com/haskell/haskell-language-server/pull/2763)) by @georgefst
- Fix Change Type Signature Plugin test suite for 9.2.1
([#2761](https://github.com/haskell/haskell-language-server/pull/2761)) by @drsooch
- Bump actions/checkout from 2 to 3
([#2759](https://github.com/haskell/haskell-language-server/pull/2759)) by @dependabot[bot]
- Refactor LSP logger and log via window/logMessage also
([#2758](https://github.com/haskell/haskell-language-server/pull/2758)) by @michaelpj
- Fix the tower of Babel
([#2757](https://github.com/haskell/haskell-language-server/pull/2757)) by @hasufell
- Implement cycle detection in hls-graph
([#2756](https://github.com/haskell/haskell-language-server/pull/2756)) by @pepeiborra
- Adjust rendering of error logs and drop unneeded MonadUnliftIO instance
([#2755](https://github.com/haskell/haskell-language-server/pull/2755)) by @pepeiborra
- Estimate file versions safely
([#2753](https://github.com/haskell/haskell-language-server/pull/2753)) by @pepeiborra
- Fix test failure for AlternateNumberFormat
([#2752](https://github.com/haskell/haskell-language-server/pull/2752)) by @drsooch
- LSP window message log recorder
([#2750](https://github.com/haskell/haskell-language-server/pull/2750)) by @pepeiborra
- Fix FreeBSD bindist build
([#2748](https://github.com/haskell/haskell-language-server/pull/2748)) by @hasufell
- Improve bindist makefile
([#2746](https://github.com/haskell/haskell-language-server/pull/2746)) by @hasufell
- Fix flake.lock
([#2743](https://github.com/haskell/haskell-language-server/pull/2743)) by @michaelpj
- Add failing test for variables in hovers
([#2742](https://github.com/haskell/haskell-language-server/pull/2742)) by @michaelpj
- Update Define Function Code Action to have knowledge of comments
([#2740](https://github.com/haskell/haskell-language-server/pull/2740)) by @drsooch
- Upgrade to hie-bios 0.9.0
([#2738](https://github.com/haskell/haskell-language-server/pull/2738)) by @fendor
- Track file versions accurately.
([#2735](https://github.com/haskell/haskell-language-server/pull/2735)) by @wz1000
- Fix hls-class-plugin on ghc-9.2
([#2733](https://github.com/haskell/haskell-language-server/pull/2733)) by @July541
- Bump actions/github-script from 2 to 6
([#2730](https://github.com/haskell/haskell-language-server/pull/2730)) by @dependabot[bot]
- Delete the Telemetry log level
([#2727](https://github.com/haskell/haskell-language-server/pull/2727)) by @michaelpj
- Tone down logging of plugin rules
([#2723](https://github.com/haskell/haskell-language-server/pull/2723)) by @pepeiborra
- Troubleshooting: GHC 9.2 partial support
([#2722](https://github.com/haskell/haskell-language-server/pull/2722)) by @andys8
- Remove `getHspecFormattedConfig` which is no longer used
([#2721](https://github.com/haskell/haskell-language-server/pull/2721)) by @hololeap
- Fix crash for non-LSP modes wrt #2627
([#2719](https://github.com/haskell/haskell-language-server/pull/2719)) by @hasufell
- Wingman: Don't use keywords for variable names
([#2717](https://github.com/haskell/haskell-language-server/pull/2717)) by @isovector
- Expose DisplayTHWarning (backport #2712)
([#2714](https://github.com/haskell/haskell-language-server/pull/2714)) by @mergify[bot]
- Send LSP error when GHC cannot be found
([#2713](https://github.com/haskell/haskell-language-server/pull/2713)) by @hasufell
- Expose DisplayTHWarning
([#2712](https://github.com/haskell/haskell-language-server/pull/2712)) by @pepeiborra
- Improve wrapper cradle errors
([#2711](https://github.com/haskell/haskell-language-server/pull/2711)) by @hasufell
- Fix min bound for ghc-exactprint dependency in hls-class-plugin
([#2710](https://github.com/haskell/haskell-language-server/pull/2710)) by @pepeiborra
- Remove duplicate help messages & format CRLF to LF
([#2709](https://github.com/haskell/haskell-language-server/pull/2709)) by @July541
- Add @July541 for call-hierarchy-plugin
([#2708](https://github.com/haskell/haskell-language-server/pull/2708)) by @July541
- Fix releasing
([#2707](https://github.com/haskell/haskell-language-server/pull/2707)) by @hasufell
- Print info message when ignoring a file due to a none cradle
([#2701](https://github.com/haskell/haskell-language-server/pull/2701)) by @ThomasCrevoisier
- fix: handle comma in extend import list with ghc 9.2
([#2697](https://github.com/haskell/haskell-language-server/pull/2697)) by @guibou
- Build Alternate Number Format Plugin with GHC 9.2
([#2696](https://github.com/haskell/haskell-language-server/pull/2696)) by @drsooch
- Optionally publish packages definitely in the hackage workflow
([#2689](https://github.com/haskell/haskell-language-server/pull/2689)) by @jneira
- Set -dynamic in cabal.project
([#2688](https://github.com/haskell/haskell-language-server/pull/2688)) by @jneira
- Multi component issues in GHC 9.2
([#2687](https://github.com/haskell/haskell-language-server/pull/2687)) by @pepeiborra
- Fix flaky boot def test
([#2686](https://github.com/haskell/haskell-language-server/pull/2686)) by @eddiemundo
- Fix typos in troubleshooting.md
([#2680](https://github.com/haskell/haskell-language-server/pull/2680)) by @visortelle
- Add pre-commit hook for cleaning up mixed-line endings
([#2679](https://github.com/haskell/haskell-language-server/pull/2679)) by @drsooch
- Add a test for #2673
([#2676](https://github.com/haskell/haskell-language-server/pull/2676)) by @pepeiborra
- Implement distribution of dynamic builds
([#2675](https://github.com/haskell/haskell-language-server/pull/2675)) by @hasufell
- Restore eval plugin build for GHC 9.2
([#2669](https://github.com/haskell/haskell-language-server/pull/2669)) by @guibou
- Change Type Signature Plugin
([#2660](https://github.com/haskell/haskell-language-server/pull/2660)) by @drsooch
- Nix flake fix dev shells
([#2655](https://github.com/haskell/haskell-language-server/pull/2655)) by @guibou
- Speed up fuzzy search
([#2639](https://github.com/haskell/haskell-language-server/pull/2639)) by @Bodigrim
- Improve logging
([#2558](https://github.com/haskell/haskell-language-server/pull/2558)) by @eddiemundo
- Improve recompilation avoidance in the presence of TH
([#2316](https://github.com/haskell/haskell-language-server/pull/2316)) by @wz1000

## 1.6.1.1 (*only hackage release*)

- Release to update haskell-language-server.cabal in hackage, setting the build for the executable component as dynamically linked
  - The motivation is build by default a hls executable which works for Template Haskell
  - This doesn't need a full release cause it does not affect release executables which continue being fully static

### Pull requests merged for 1.6.1.1

- Prepare 1.6.1.1 (only hackage release)
([#2681](https://github.com/haskell/haskell-language-server/pull/2681)) by @jneira
- Add the -dynamic flag and update build instructions
([#2668](https://github.com/haskell/haskell-language-server/pull/2668)) by @pepeiborra

## 1.6.1.0

This is a bug fix release to restore a fully statically linked haskell-language-server-wrapper executable.

- It has been reported [here](https://github.com/haskell/haskell-language-server/issues/2650)
  - Thanks all reporters for the fast feedback
- The bug has been traced [here](https://github.com/haskell/haskell-language-server/pull/2615#discussion_r795059782)
- And the fix is in [this pr](https://github.com/haskell/haskell-language-server/pull/2647)

### Pull requests merged for 1.6.1.0

- Post 1.6.0.0 fixes and prepare 1.6.1.0 bug fix release
([#2647](https://github.com/haskell/haskell-language-server/pull/2647)) by @jneira
- Move hackage back to flake.nix
([#2652](https://github.com/haskell/haskell-language-server/pull/2652)) by @guibou
- Wingman: Fix #1879
([#2644](https://github.com/haskell/haskell-language-server/pull/2644)) by @MorrowM


## 1.6.0.0

Time for a new and exciting hls release:

- It includes *three* brand new plugins:
  - [Alternate number literals](https://haskell-language-server.readthedocs.io/en/latest/features.html#convert-numbers-to-alternative-formats) thanks to @drsooch
  - [Qualify imported names](https://haskell-language-server.readthedocs.io/en/latest/features.html#qualify-imported-names) thanks to @eddiemundo
  - New plugin to support [selection range](https://haskell-language-server.readthedocs.io/en/latest/features.html#selection-range) (aka double click text selection) thanks to @kokobd
- Finally hls [supports *ghc 9.2.1*](https://github.com/haskell/haskell-language-server/issues/2179)
  - Including core features and [many plugins](https://haskell-language-server.readthedocs.io/en/latest/supported-versions.html#plugins-support-by-ghc-version)
  - Thanks to a great collective effort coordinated by @pepeiborra and with the help of @wz1000, @mpickering and @alanz among others
- Hls now also [supports *ghc 9.0.2*](https://github.com/haskell/haskell-language-server/issues/297) with all plugins but the stylish-haskell formatter
  - Including the [wingman plugin](https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-tactics-plugin) thanks to @isovector and @anka-213
- And many many fixes and performance improvements, thanks to all contributors!

### Deprecation notice for 1.6.0

- As we noted in the previous release we have dropped support for ghc versions 8.10.5 and 8.8.3 in *this release*
  - We recommend upgrading ghc to the last minor version: 8.8.4 or 8.10.7
  - You can read more about ghc deprecation policy and schedule [here](https://haskell-language-server.readthedocs.io/en/latest/supported-versions.html)
- *After this release*:
  - [We will remove all project stack.yaml's](https://github.com/haskell/haskell-language-server/issues/2533) but two: one for last lts and other for nightly. Temporary we could keep one more stack yaml when nightly upgrades the ghc version, to help in the transition
  - [We will remove the install script](https://github.com/haskell/haskell-language-server/issues/2491) which lives [here](https://github.com/haskell/haskell-language-server/tree/master/install)
    - If you want to install hls from source we recommend using `ghcup`. Download it and run `ghcup compile hls --help` to get more info about.

### Pull requests merged for 1.6.0

- Prepare 1.6.0 release
([#2642](https://github.com/haskell/haskell-language-server/pull/2642)) by @jneira
- Implement stripPrefix via T.stripPrefix
([#2645](https://github.com/haskell/haskell-language-server/pull/2645)) by @Bodigrim
- Change Type Family Export pattern
([#2643](https://github.com/haskell/haskell-language-server/pull/2643)) by @drsooch
- Disable alpine build by default
([#2638](https://github.com/haskell/haskell-language-server/pull/2638)) by @jneira
- Use T.decodeUtf8 + BS.readFile instead of T.readFile
([#2637](https://github.com/haskell/haskell-language-server/pull/2637)) by @Bodigrim
- Add ghc 9.2.1 to gitlab ci
([#2636](https://github.com/haskell/haskell-language-server/pull/2636)) by @jneira
- Specialize ghcide indent style to .hs
([#2631](https://github.com/haskell/haskell-language-server/pull/2631)) by @mrgutkun
- Fix off by one indexing error in openingBacktick
([#2629](https://github.com/haskell/haskell-language-server/pull/2629)) by @pepeiborra
- Drop bytestring-encoding
([#2628](https://github.com/haskell/haskell-language-server/pull/2628)) by @pepeiborra
- fix positionInRange
([#2625](https://github.com/haskell/haskell-language-server/pull/2625)) by @kokobd
- Fix #2612 - hlint plugin - Apply fixities to parsed source before sending to apply-refact
([#2624](https://github.com/haskell/haskell-language-server/pull/2624)) by @eddiemundo
- Flake ghc 92
([#2621](https://github.com/haskell/haskell-language-server/pull/2621)) by @guibou
- Use ghc+integer-gmp for alpine linux build release
([#2615](https://github.com/haskell/haskell-language-server/pull/2615)) by @jneira
- Use helpers from lsp to do code action prefixing
([#2614](https://github.com/haskell/haskell-language-server/pull/2614)) by @michaelpj
- Wingman: Fix fundeps
([#2611](https://github.com/haskell/haskell-language-server/pull/2611)) by @isovector
- Wingman idioms
([#2607](https://github.com/haskell/haskell-language-server/pull/2607)) by @isovector
- Make work stack-9.2.1.yaml and enable `pedantic` (`-WError`) for cabal
([#2606](https://github.com/haskell/haskell-language-server/pull/2606)) by @jneira
- Improve qualified import plugin readme
([#2605](https://github.com/haskell/haskell-language-server/pull/2605)) by @eddiemundo
- Correct typo in Ide.Arguments:listPluginsParser
([#2604](https://github.com/haskell/haskell-language-server/pull/2604)) by @tombusby
- Rework features documentation
([#2603](https://github.com/haskell/haskell-language-server/pull/2603)) by @michaelpj
- [ghc-9.2] Fix refine-imports plugin
([#2601](https://github.com/haskell/haskell-language-server/pull/2601)) by @mrgutkun
- [ghc-9.2] Fix qualify-imported-names plugin
([#2600](https://github.com/haskell/haskell-language-server/pull/2600)) by @mrgutkun
- Correct issues with pre-commit hook
([#2597](https://github.com/haskell/haskell-language-server/pull/2597)) by @bradrn
- Fix some import module completions being dropped (and fix flaky test too)
([#2595](https://github.com/haskell/haskell-language-server/pull/2595)) by @eddiemundo
- Fix module-name plugin on ghc-9.2.1
([#2594](https://github.com/haskell/haskell-language-server/pull/2594)) by @mrgutkun
- [ghc-9.2] Fix rename plugin
([#2593](https://github.com/haskell/haskell-language-server/pull/2593)) by @pepeiborra
- Fix progress eval test randomly failing
([#2590](https://github.com/haskell/haskell-language-server/pull/2590)) by @eddiemundo
- More work around next ghc-9.2.1 support
([#2587](https://github.com/haskell/haskell-language-server/pull/2587)) by @jneira
- Post ghc-9.2.1 config cleanup
([#2582](https://github.com/haskell/haskell-language-server/pull/2582)) by @jneira
- GHC-9.0 support for hls-tactics-plugin
([#2581](https://github.com/haskell/haskell-language-server/pull/2581)) by @isovector
- Wingman: Fix TODO(sandy) when performing subsequent actions
([#2580](https://github.com/haskell/haskell-language-server/pull/2580)) by @isovector
- Bump Ormolu and Fourmolu to GHC-9.2-compatible versions
([#2579](https://github.com/haskell/haskell-language-server/pull/2579)) by @georgefst
- test: Add regression tests for #2403
([#2576](https://github.com/haskell/haskell-language-server/pull/2576)) by @guibou
- Fix crash on completion with type family
([#2569](https://github.com/haskell/haskell-language-server/pull/2569)) by @guibou
- Add support for ghc 9.0.2
([#2567](https://github.com/haskell/haskell-language-server/pull/2567)) by @jneira
- support selection range lsp feature
([#2565](https://github.com/haskell/haskell-language-server/pull/2565)) by @kokobd
- Reuse build setup using a dedicated github action
([#2563](https://github.com/haskell/haskell-language-server/pull/2563)) by @jneira
- Fix ci update hackage index
([#2562](https://github.com/haskell/haskell-language-server/pull/2562)) by @jneira
- Enable `aarch64-darwin` in `flake.nix`
([#2561](https://github.com/haskell/haskell-language-server/pull/2561)) by @Gabriel439
- Fix freeze cache key correctly
([#2560](https://github.com/haskell/haskell-language-server/pull/2560)) by @jneira
- Fix nix flake by explicit version for `lsp-xxx` packages
([#2557](https://github.com/haskell/haskell-language-server/pull/2557)) by @guibou
- Apply missing update for stack-9.0.1.yaml
([#2556](https://github.com/haskell/haskell-language-server/pull/2556)) by @Ailrun
- doc: Enable relative links with anchors
([#2555](https://github.com/haskell/haskell-language-server/pull/2555)) by @sir4ur0n
- Fix space leak where EPS retained HPTs from old HscEnv
([#2553](https://github.com/haskell/haskell-language-server/pull/2553)) by @mpickering
- Remove cabal.project.freeze files in workflows after computing the cache key
([#2552](https://github.com/haskell/haskell-language-server/pull/2552)) by @jneira
- Add support for brittany (needs aeson-2) and floskell with ghc-9.0.1
([#2551](https://github.com/haskell/haskell-language-server/pull/2551)) by @jneira
- Restore TemplateHaskell pragma in hls-graph
([#2549](https://github.com/haskell/haskell-language-server/pull/2549)) by @pepeiborra
- Add space after comma when exporting a name
([#2547](https://github.com/haskell/haskell-language-server/pull/2547)) by @sergv
- Set an unique name for Hlint job
([#2544](https://github.com/haskell/haskell-language-server/pull/2544)) by @jneira
- Fix ghcide handling project root
([#2543](https://github.com/haskell/haskell-language-server/pull/2543)) by @drsooch
- CI: linting
([#2538](https://github.com/haskell/haskell-language-server/pull/2538)) by @Anton-Latukha
- CI: add hlint workflow
([#2537](https://github.com/haskell/haskell-language-server/pull/2537)) by @Anton-Latukha
- CI: caching: closer match work/CI guarantees
([#2536](https://github.com/haskell/haskell-language-server/pull/2536)) by @Anton-Latukha
- CI: caching: keep-going
([#2535](https://github.com/haskell/haskell-language-server/pull/2535)) by @Anton-Latukha
- CI: {caching,test,bench}: mk cache aware of package dep versions
([#2532](https://github.com/haskell/haskell-language-server/pull/2532)) by @Anton-Latukha
- Test hls-pragmas-plugin in ci
([#2530](https://github.com/haskell/haskell-language-server/pull/2530)) by @jneira
- Enable manual run for caching, hackage and build workflows
([#2528](https://github.com/haskell/haskell-language-server/pull/2528)) by @jneira
- Fix random SQLite busy database is locked errors
([#2527](https://github.com/haskell/haskell-language-server/pull/2527)) by @eddiemundo
- Fix some hlint warnings
([#2523](https://github.com/haskell/haskell-language-server/pull/2523)) by @jhrcek
- Improve action for fixing import typo
([#2522](https://github.com/haskell/haskell-language-server/pull/2522)) by @jhrcek
- CI: caching: fix early termination expression check & cabal.project replacement
([#2520](https://github.com/haskell/haskell-language-server/pull/2520)) by @Anton-Latukha
- Solve crash with module name plugin under certain circumstances
([#2518](https://github.com/haskell/haskell-language-server/pull/2518)) by @ttylec
- Rework troubleshooting section, add basic explainer
([#2517](https://github.com/haskell/haskell-language-server/pull/2517)) by @michaelpj
- Refactor collectLiterals in AlternateNumberFormat.
([#2516](https://github.com/haskell/haskell-language-server/pull/2516)) by @drsooch
- cabal-*.project: index-state +1s
([#2515](https://github.com/haskell/haskell-language-server/pull/2515)) by @Anton-Latukha
- Bump up retrie
([#2513](https://github.com/haskell/haskell-language-server/pull/2513)) by @jneira
- Sort out some compatibility issues
([#2511](https://github.com/haskell/haskell-language-server/pull/2511)) by @alanz
- Fix ci cache for windows
([#2507](https://github.com/haskell/haskell-language-server/pull/2507)) by @jneira
- CI: caching: add early termination & run check on schedule
([#2506](https://github.com/haskell/haskell-language-server/pull/2506)) by @Anton-Latukha
- Fix tracing of recordDirtyKeys
([#2505](https://github.com/haskell/haskell-language-server/pull/2505)) by @pepeiborra
- Unhandled exceptions fixed
([#2504](https://github.com/haskell/haskell-language-server/pull/2504)) by @pepeiborra
- Build with GHC 9.2
([#2503](https://github.com/haskell/haskell-language-server/pull/2503)) by @pepeiborra
- Ignore stack.yamls in test cabal workflow
([#2502](https://github.com/haskell/haskell-language-server/pull/2502)) by @jneira
- small stack yaml updates to ease maintenance
([#2501](https://github.com/haskell/haskell-language-server/pull/2501)) by @simonmichael
- Automatically read in the doc version from the cabal file
([#2500](https://github.com/haskell/haskell-language-server/pull/2500)) by @michaelpj
- Disable alternate numbers format plugin temporary
([#2498](https://github.com/haskell/haskell-language-server/pull/2498)) by @jneira
- Revert "Send unhandled exceptions to the user (#2484)"
([#2497](https://github.com/haskell/haskell-language-server/pull/2497)) by @jneira
- Upgrade to new version of lsp libraries
([#2494](https://github.com/haskell/haskell-language-server/pull/2494)) by @michaelpj
- Fail if main or pre jobs are cancelled
([#2493](https://github.com/haskell/haskell-language-server/pull/2493)) by @jneira
- stack-9.0.1: update/cleanup
([#2489](https://github.com/haskell/haskell-language-server/pull/2489)) by @simonmichael
- Correctly handle LSP shutdown/exit
([#2486](https://github.com/haskell/haskell-language-server/pull/2486)) by @pepeiborra
- Fix hls-graph ide build with embed-files
([#2485](https://github.com/haskell/haskell-language-server/pull/2485)) by @pepeiborra
- Send unhandled exceptions to the user
([#2484](https://github.com/haskell/haskell-language-server/pull/2484)) by @pepeiborra
- Fix redundant import actions for names starting with _
([#2483](https://github.com/haskell/haskell-language-server/pull/2483)) by @Ailrun
- Update flake to use fourmolu plugin in GHC 9
([#2482](https://github.com/haskell/haskell-language-server/pull/2482)) by @Ailrun
- Delete some dead or deprecated settings
([#2481](https://github.com/haskell/haskell-language-server/pull/2481)) by @michaelpj
- Class plugin bump up
([#2475](https://github.com/haskell/haskell-language-server/pull/2475)) by @Ailrun
- Fix some pragma completion cases
([#2474](https://github.com/haskell/haskell-language-server/pull/2474)) by @Ailrun
- Minor org to contribution doc
([#2472](https://github.com/haskell/haskell-language-server/pull/2472)) by @Anton-Latukha
- Warn if TH and Mac and static binary
([#2470](https://github.com/haskell/haskell-language-server/pull/2470)) by @pepeiborra
- Lock-less debouncer (minimal change)
([#2469](https://github.com/haskell/haskell-language-server/pull/2469)) by @pepeiborra
- Handle re-exported modules when constructing ExportsMap
([#2468](https://github.com/haskell/haskell-language-server/pull/2468)) by @jhrcek
- Caching process update
([#2467](https://github.com/haskell/haskell-language-server/pull/2467)) by @Anton-Latukha
- #2418 Also use .hlint.yaml fixity rules when HLINT_ON_LIB_GHC not defined
([#2464](https://github.com/haskell/haskell-language-server/pull/2464)) by @eddiemundo
- Build linux binaries in alpine container
([#2463](https://github.com/haskell/haskell-language-server/pull/2463)) by @pepeiborra
- Lockless iorefs
([#2460](https://github.com/haskell/haskell-language-server/pull/2460)) by @pepeiborra
- Join nested IO actions of the form `IO (IO ())`
([#2459](https://github.com/haskell/haskell-language-server/pull/2459)) by @fendor
- #600 Code action to ignore hlint hints module wide
([#2458](https://github.com/haskell/haskell-language-server/pull/2458)) by @eddiemundo
- lock-less progress-reporting
([#2453](https://github.com/haskell/haskell-language-server/pull/2453)) by @pepeiborra
- Fix the nix build
([#2452](https://github.com/haskell/haskell-language-server/pull/2452)) by @michaelpj
- Fix rerun log cache handling
([#2450](https://github.com/haskell/haskell-language-server/pull/2450)) by @jneira
- Make heavy use of common sections
([#2447](https://github.com/haskell/haskell-language-server/pull/2447)) by @fendor
- CI: organizing bootstraping
([#2446](https://github.com/haskell/haskell-language-server/pull/2446)) by @Anton-Latukha
- Describe hls installed binaries
([#2445](https://github.com/haskell/haskell-language-server/pull/2445)) by @jneira
- Remove support for ghc 8.8.3/8.10.5
([#2444](https://github.com/haskell/haskell-language-server/pull/2444)) by @jneira
- CI: cabal 3.6 use & clean-up 8.10.5 builds
([#2443](https://github.com/haskell/haskell-language-server/pull/2443)) by @Anton-Latukha
- Lockless FileExistsMap and position mapping
([#2442](https://github.com/haskell/haskell-language-server/pull/2442)) by @pepeiborra
- Fix regression in Eval plugin and add test
([#2441](https://github.com/haskell/haskell-language-server/pull/2441)) by @pepeiborra
- Makes local record field completion respects the fields sharing one single type signature
([#2439](https://github.com/haskell/haskell-language-server/pull/2439)) by @konn
- Enable top-level hover signature test
([#2435](https://github.com/haskell/haskell-language-server/pull/2435)) by @jneira
- Lockless diagnostics
([#2434](https://github.com/haskell/haskell-language-server/pull/2434)) by @pepeiborra
- Move Common Plugin Functions into PluginUtils
([#2433](https://github.com/haskell/haskell-language-server/pull/2433)) by @drsooch
- lock-less Values state
([#2429](https://github.com/haskell/haskell-language-server/pull/2429)) by @pepeiborra
- Extract the pre-decl pragma parsing to its own module
([#2428](https://github.com/haskell/haskell-language-server/pull/2428)) by @eddiemundo
- CI: cache-deps: rm pull request hook
([#2426](https://github.com/haskell/haskell-language-server/pull/2426)) by @Anton-Latukha
- Add known broken tests for import placement
([#2425](https://github.com/haskell/haskell-language-server/pull/2425)) by @nini-faroux
- Use stm-stats to reduce contention in hls-graph
([#2421](https://github.com/haskell/haskell-language-server/pull/2421)) by @pepeiborra
- Build on FreeBSD12 only
([#2420](https://github.com/haskell/haskell-language-server/pull/2420)) by @hasufell
- Centralized caching workflow
([#2419](https://github.com/haskell/haskell-language-server/pull/2419)) by @Anton-Latukha
- Configuration docs: Typo
([#2417](https://github.com/haskell/haskell-language-server/pull/2417)) by @andys8
- Use dependent-sum from hackage
([#2412](https://github.com/haskell/haskell-language-server/pull/2412)) by @jneira
- Lock-less hls-graph
([#2411](https://github.com/haskell/haskell-language-server/pull/2411)) by @pepeiborra
- hls-graph.cabal: link to actual readme
([#2404](https://github.com/haskell/haskell-language-server/pull/2404)) by @juhp
- Disable check project in the ghcide test suite
([#2397](https://github.com/haskell/haskell-language-server/pull/2397)) by @pepeiborra
- Add modern issue templates
([#2394](https://github.com/haskell/haskell-language-server/pull/2394)) by @jneira
- Fix extension pragma inserted below ghc options pragma #2364
([#2392](https://github.com/haskell/haskell-language-server/pull/2392)) by @eddiemundo
- Avoid unnecessary Target canonicalisation in Session setup
([#2359](https://github.com/haskell/haskell-language-server/pull/2359)) by @fendor
- Decrease contention in Progress reporting
([#2357](https://github.com/haskell/haskell-language-server/pull/2357)) by @pepeiborra
- Qualify imported names plugin
([#2355](https://github.com/haskell/haskell-language-server/pull/2355)) by @eddiemundo
- HLS Plugin to provide Alternate Literal Formats.
([#2350](https://github.com/haskell/haskell-language-server/pull/2350)) by @drsooch
- Log live_bytes and heap_size as reported by GHC.Stats
([#1508](https://github.com/haskell/haskell-language-server/pull/1508)) by @mpickering

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
