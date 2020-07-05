# Changelog for haskell-language-server

## 0.2

- Use cabal-plan from Hackage
([#185](https://github.com/haskell/haskell-ide-engine/pull/185) by @georgefst)

- Bump ghcide to wz1000 hls-2 branch
([#184](https://github.com/haskell/haskell-ide-engine/pull/184) by @alanz)

- doc(preprocessor): Document the preprocessor limitation
([#177](https://github.com/haskell/haskell-ide-engine/pull/177) by @Sir4ur0n)

- Use shell.nix from Haskell-IDE-Engine
([#169](https://github.com/haskell/haskell-ide-engine/pull/169) by @fendor)

- Remove last occurrences of shake.yaml
([#163](https://github.com/haskell/haskell-ide-engine/pull/163) by @fendor)

- Use an unique install/stack.yaml
([#154](https://github.com/haskell/haskell-ide-engine/pull/154) by @jneira)

- Introduce golden testing
([#152](https://github.com/haskell/haskell-ide-engine/pull/152) by @Ailrun)

- Revert "Use bullet as separator instead of HR"
([#150](https://github.com/haskell/haskell-ide-engine/pull/150) by @alanz)

- feat(hie-bios): Multi-cradle, ignore directories
([#147](https://github.com/haskell/haskell-ide-engine/pull/147) by @Sir4ur0n)

- [Plugin] stylish-haskell formatter
([#146](https://github.com/haskell/haskell-ide-engine/pull/146) by @Ailrun)

- Separate ghcide tests and disable them for now
([#137](https://github.com/haskell/haskell-ide-engine/pull/137) by @jneira)

- Convert private lib in common stanza
([#136](https://github.com/haskell/haskell-ide-engine/pull/136) by @jneira)

- Add zlibc to readme
([#134](https://github.com/haskell/haskell-ide-engine/pull/134) by @Sir4ur0n)

- Complete editor integrations
([#132](https://github.com/haskell/haskell-ide-engine/pull/132) by @jneira)

- Remove inexistent component from hie.yaml.stack
([#131](https://github.com/haskell/haskell-ide-engine/pull/131) by @jneira)

- Bump to new mpickering/ghcide
([#130](https://github.com/haskell/haskell-ide-engine/pull/130) by @alanz)

- Update ghc-lib-parser version
([#129](https://github.com/haskell/haskell-ide-engine/pull/129) by @jneira)

- Remove redundant import
([#128](https://github.com/haskell/haskell-ide-engine/pull/128) by @bubba)

- Default the number of Shake threads to 0 (automatic)
([#127](https://github.com/haskell/haskell-ide-engine/pull/127) by @bubba)

- Added kakoune integration instructions
([#125](https://github.com/haskell/haskell-ide-engine/pull/125) by @414owen)

- Fix install script dev target
([#124](https://github.com/haskell/haskell-ide-engine/pull/124) by @jneira)

- Add plugin support for Rename providers
([#123](https://github.com/haskell/haskell-ide-engine/pull/123) by @pepeiborra)

- Add jobs for stack and cabal using ghc-8.10.1
([#120](https://github.com/haskell/haskell-ide-engine/pull/120) by @jneira)

- Add lower bound to tasty-ant-xml
([#119](https://github.com/haskell/haskell-ide-engine/pull/119) by @jneira)

- Fix build using brittany revision
([#117](https://github.com/haskell/haskell-ide-engine/pull/117) by @jneira)

- Use floskell released version 0.10.3
([#116](https://github.com/haskell/haskell-ide-engine/pull/116) by @jneira)

- Add emacs/doom-emacs integration sub-section
([#115](https://github.com/haskell/haskell-ide-engine/pull/115) by @yuanw)

- Port hie README partially
([#112](https://github.com/haskell/haskell-ide-engine/pull/112) by @jneira)

- Use cabal-helper-1.1, add stack-8.10.1.yaml and unify cabal.project's
([#108](https://github.com/haskell/haskell-ide-engine/pull/108) by @jneira)

- [#87] Fix completion via ghcide's `getCompletionsLSP`
([#107](https://github.com/haskell/haskell-ide-engine/pull/107) by @korayal)

- Create specific project file for ghc-8.10.
([#106](https://github.com/haskell/haskell-ide-engine/pull/106) by @jneira)

- Issue 5 - Move HIE Tests and convert to Tasty
([#105](https://github.com/haskell/haskell-ide-engine/pull/105) by @jeffwindsor)

- Hls update latest hie bios
([#100](https://github.com/haskell/haskell-ide-engine/pull/100) by @fendor)

- Update extra-deps to use latest fork version of shake
([#98](https://github.com/haskell/haskell-ide-engine/pull/98) by @fendor)

- Activate typechecking in non-lsp mode
([#95](https://github.com/haskell/haskell-ide-engine/pull/95) by @jneira)

- Fix haddock parsing errors
([#92](https://github.com/haskell/haskell-ide-engine/pull/92) by @jneira)

- Update for haskell-lsp 0.22
([#89](https://github.com/haskell/haskell-ide-engine/pull/89) by @alanz)

- Get building with ghc-8.10
([#83](https://github.com/haskell/haskell-ide-engine/pull/83) by @bubba)

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
