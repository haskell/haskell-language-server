# Changelog for haskell-language-server

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
([#255](https://github.com/haskell/haskell-language-server/pull/255) by @georgefst)
- Ormolu flags
([#246](https://github.com/haskell/haskell-language-server/pull/246) by @pepeiborra)
- Ormolu fix
([#257](https://github.com/haskell/haskell-language-server/pull/257) by @sureyeaah)
- Remove redundant CircleCI steps
([#259](https://github.com/haskell/haskell-language-server/pull/259) by @bubba)
- Slow down Tasty by limiting it to -j1
([#261](https://github.com/haskell/haskell-language-server/pull/261) by @bubba)
- Remove hspec-expectations
([#260](https://github.com/haskell/haskell-language-server/pull/260) by @bubba)
- Remove a redundant caching step
([#262](https://github.com/haskell/haskell-language-server/pull/262) by @Ailrun)
- add hie.yaml to coc configuration
([#267](https://github.com/haskell/haskell-language-server/pull/267) by @sureyeaah)
- Initial Retrie plugin
([#266](https://github.com/haskell/haskell-language-server/pull/266) by @pepeiborra)
- Add exe extension to win executables
([#284](https://github.com/haskell/haskell-language-server/pull/284) by @jneira)
- Use wz1000/hls-3 ghcide branch
([#275](https://github.com/haskell/haskell-language-server/pull/275) by @alanz)
- Fix rename capability being declared
([#285](https://github.com/haskell/haskell-language-server/pull/285) by @bubba)
- Add CI job for 8.8.4
([#287](https://github.com/haskell/haskell-language-server/pull/287) by @bubba)
- Make the AGPL flag manual in cabal
([#250](https://github.com/haskell/haskell-language-server/pull/250) by @fendor)
- Bring in doc URL fix for Windows
([#289](https://github.com/haskell/haskell-language-server/pull/289) by @bubba)
- Bring in fix for libm on Linux static binaries
([#293](https://github.com/haskell/haskell-language-server/pull/293) by @bubba)
- Add fourmolu plugin (attempt 2) and add Brittany for ghc-8.10.1
([#264](https://github.com/haskell/haskell-language-server/pull/264) by @georgefst)
- Trying new hls-3 branch
([#300](https://github.com/haskell/haskell-language-server/pull/300) by @alanz)

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

### Pull requests merged

- Mention docs on hover feature in README
([#209](https://github.com/haskell/haskell-language-server/pull/209) by @georgefst)
- Add static binaries for ghc-8.8.4
([#224](https://github.com/haskell/haskell-language-server/pull/224) by @bubba)
- Rename the configuration section from languageServerHaskell => haskell
([#227](https://github.com/haskell/haskell-language-server/pull/227) by @bubba)
- Use -haddock for cabal and stack
([#214](https://github.com/haskell/haskell-language-server/pull/214) by @jneira)
- slightly better shell.nix for local development
([#235](https://github.com/haskell/haskell-language-server/pull/235) by @pepeiborra)
- Shell nix further steps
([#240](https://github.com/haskell/haskell-language-server/pull/240) by @pepeiborra)
- Add numeric-version option for wrapper and server
([#241](https://github.com/haskell/haskell-language-server/pull/241) by @fendor)
- Accept the legacy "languageServerHaskell" config name
([#243](https://github.com/haskell/haskell-language-server/pull/243) by @bubba)
- Fix for Eval plugin: Error from tests not reported
([#244](https://github.com/haskell/haskell-language-server/pull/244) by @tittoassini)
- Rename binaries before uploading
([#248](https://github.com/haskell/haskell-language-server/pull/248) by @bubba)

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

### Pull requests merged

- Bump ormolu to 0.1.2.0
([#189](https://github.com/haskell/haskell-language-server/pull/189) by @AlistairB)
- Remove dependency on Cabal
([#195](https://github.com/haskell/haskell-language-server/pull/195) by @bubba)
- Fix extraneous extra-dep in stack-8.6.4.yaml
([#199](https://github.com/haskell/haskell-language-server/pull/199) by @bubba)
- Fix install script stack targets
([#203](https://github.com/haskell/haskell-language-server/pull/203) by @jneira)
- Add support for ghc-8.8.4
([#206](https://github.com/haskell/haskell-language-server/pull/206) by @jneira)
- Simple Eval plugin
([#191](https://github.com/haskell/haskell-language-server/pull/191) by @pepeiborra)
- Distributable binaries
([#165](https://github.com/haskell/haskell-language-server/pull/165) by @bubba)

## 0.2

- Use cabal-plan from Hackage
([#185](https://github.com/haskell/haskell-language-server/pull/185) by @georgefst)

- Bump ghcide to wz1000 hls-2 branch
([#184](https://github.com/haskell/haskell-language-server/pull/184) by @alanz)

- doc(preprocessor): Document the preprocessor limitation
([#177](https://github.com/haskell/haskell-language-server/pull/177) by @Sir4ur0n)

- Use shell.nix from Haskell-IDE-Engine
([#169](https://github.com/haskell/haskell-language-server/pull/169) by @fendor)

- Remove last occurrences of shake.yaml
([#163](https://github.com/haskell/haskell-language-server/pull/163) by @fendor)

- Use an unique install/stack.yaml
([#154](https://github.com/haskell/haskell-language-server/pull/154) by @jneira)

- Introduce golden testing
([#152](https://github.com/haskell/haskell-language-server/pull/152) by @Ailrun)

- Revert "Use bullet as separator instead of HR"
([#150](https://github.com/haskell/haskell-language-server/pull/150) by @alanz)

- feat(hie-bios): Multi-cradle, ignore directories
([#147](https://github.com/haskell/haskell-language-server/pull/147) by @Sir4ur0n)

- [Plugin] stylish-haskell formatter
([#146](https://github.com/haskell/haskell-language-server/pull/146) by @Ailrun)

- Separate ghcide tests and disable them for now
([#137](https://github.com/haskell/haskell-language-server/pull/137) by @jneira)

- Convert private lib in common stanza
([#136](https://github.com/haskell/haskell-language-server/pull/136) by @jneira)

- Add zlibc to readme
([#134](https://github.com/haskell/haskell-language-server/pull/134) by @Sir4ur0n)

- Complete editor integrations
([#132](https://github.com/haskell/haskell-language-server/pull/132) by @jneira)

- Remove inexistent component from hie.yaml.stack
([#131](https://github.com/haskell/haskell-language-server/pull/131) by @jneira)

- Bump to new mpickering/ghcide
([#130](https://github.com/haskell/haskell-language-server/pull/130) by @alanz)

- Update ghc-lib-parser version
([#129](https://github.com/haskell/haskell-language-server/pull/129) by @jneira)

- Remove redundant import
([#128](https://github.com/haskell/haskell-language-server/pull/128) by @bubba)

- Default the number of Shake threads to 0 (automatic)
([#127](https://github.com/haskell/haskell-language-server/pull/127) by @bubba)

- Added kakoune integration instructions
([#125](https://github.com/haskell/haskell-language-server/pull/125) by @414owen)

- Fix install script dev target
([#124](https://github.com/haskell/haskell-language-server/pull/124) by @jneira)

- Add plugin support for Rename providers
([#123](https://github.com/haskell/haskell-language-server/pull/123) by @pepeiborra)

- Add jobs for stack and cabal using ghc-8.10.1
([#120](https://github.com/haskell/haskell-language-server/pull/120) by @jneira)

- Add lower bound to tasty-ant-xml
([#119](https://github.com/haskell/haskell-language-server/pull/119) by @jneira)

- Fix build using brittany revision
([#117](https://github.com/haskell/haskell-language-server/pull/117) by @jneira)

- Use floskell released version 0.10.3
([#116](https://github.com/haskell/haskell-language-server/pull/116) by @jneira)

- Add emacs/doom-emacs integration sub-section
([#115](https://github.com/haskell/haskell-language-server/pull/115) by @yuanw)

- Port hie README partially
([#112](https://github.com/haskell/haskell-language-server/pull/112) by @jneira)

- Use cabal-helper-1.1, add stack-8.10.1.yaml and unify cabal.project's
([#108](https://github.com/haskell/haskell-language-server/pull/108) by @jneira)

- [#87] Fix completion via ghcide's `getCompletionsLSP`
([#107](https://github.com/haskell/haskell-language-server/pull/107) by @korayal)

- Create specific project file for ghc-8.10.
([#106](https://github.com/haskell/haskell-language-server/pull/106) by @jneira)

- Issue 5 - Move HIE Tests and convert to Tasty
([#105](https://github.com/haskell/haskell-language-server/pull/105) by @jeffwindsor)

- Hls update latest hie bios
([#100](https://github.com/haskell/haskell-language-server/pull/100) by @fendor)

- Update extra-deps to use latest fork version of shake
([#98](https://github.com/haskell/haskell-language-server/pull/98) by @fendor)

- Activate typechecking in non-lsp mode
([#95](https://github.com/haskell/haskell-language-server/pull/95) by @jneira)

- Fix haddock parsing errors
([#92](https://github.com/haskell/haskell-language-server/pull/92) by @jneira)

- Update for haskell-lsp 0.22
([#89](https://github.com/haskell/haskell-language-server/pull/89) by @alanz)

- Get building with ghc-8.10
([#83](https://github.com/haskell/haskell-language-server/pull/83) by @bubba)

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
