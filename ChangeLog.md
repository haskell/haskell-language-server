# Changelog for haskell-language-server

## 0.5.1

0.5.1 is a minor bug fix release, mainly fixing an issue with the eval plugin
as well as upgrading the ormolu and stylish-haskell dependencies.

### Pull requests merged
- Minimal fix for eval regression
([#488)](https://github.com/haskell/haskell-language-server/pull/488) by @pepeiborra
- Bump stylish-haskell to 0.12.2.0
([#482)](https://github.com/haskell/haskell-language-server/pull/482) by @maksbotan
- Improve the emacs instructions a little
([#479)](https://github.com/haskell/haskell-language-server/pull/479) by @michaelpj
- Update README: HLS is no longer in *very* early stage
([#475)](https://github.com/haskell/haskell-language-server/pull/475) by @Anrock
- Tactic plugin: Excludes Dictionary arguments in GADTs in Destruct Tactic
([#474)](https://github.com/haskell/haskell-language-server/pull/474) by @konn
- Update doom emacs install instructions in README
([#470)](https://github.com/haskell/haskell-language-server/pull/470) by @iyefrat
- Add ghc-8.10.2 to circleci
([#464)](https://github.com/haskell/haskell-language-server/pull/464) by @jneira
- Bump ormolu to 0.1.3.0
([#422)](https://github.com/haskell/haskell-language-server/pull/422) by @AlistairB

## 0.5.0

0.5.0 comes with a new tactics plugin which provides case splitting, homomorphic case splitting, and lambda introduction:

![Case splitting](https://user-images.githubusercontent.com/307223/92657198-3d4be400-f2a9-11ea-8ad3-f541c8eea891.gif)

It can even attempt to fully fill a hole!

![Attempt to fill in hole code action](https://user-images.githubusercontent.com/307223/94743611-82a18580-032c-11eb-9f13-8f46bc45f928.gif)

The imports lens plugin also learnt a new code action to make all imports explicit:

![Explicit imports code action](https://user-images.githubusercontent.com/2488460/94994815-1a53dd80-0592-11eb-8a12-ec704ae92385.gif)

There's also plenty of bug fixes, improvements and updates to the underlying tools, including Fourmolu, implicit-hie-cradle and ghcide. [Some of the improvements from ghcide](https://github.com/haskell/ghcide/releases/tag/v0.4.0) include:

* The entire project is typechecked on load
* Reverse dependencies of a module are typechecked upon saving
* Code completion includes local terms
* Import code actions now also suggest open imports
* Documentation on hover shows for symbols defined in the same module

If you're eager to try all this out, haskell-language-server is now also installable via [ghcup](https://www.haskell.org/ghcup/):

```
$ ghcup install hls
```


### Pull requests merged
- Update GHC version 8.12 to 9.0 in README
([#460)](https://github.com/haskell/haskell-language-server/pull/460) by @maralorn
- Update Fourmolu to 0.2
([#455)](https://github.com/haskell/haskell-language-server/pull/455) by @georgefst
- Generate .gz tars of all the binaries for macOS and Linux in GitHub Actions
([#454)](https://github.com/haskell/haskell-language-server/pull/454) by @bubba
- install: create hls hardlinks instead of copies except on Windows
([#451)](https://github.com/haskell/haskell-language-server/pull/451) by @juhp
- wrapper: cd to --cwd earlier
([#448)](https://github.com/haskell/haskell-language-server/pull/448) by @ocharles
- Update README.md
([#446)](https://github.com/haskell/haskell-language-server/pull/446) by @moodmosaic
- Upate Emacs setup notes
([#440)](https://github.com/haskell/haskell-language-server/pull/440) by @gdevanla
- Use ghcide master and prepare hls-plugin-api-0.4.1.0
([#439)](https://github.com/haskell/haskell-language-server/pull/439) by @jneira
- Add a code action to make all imports explicit
([#436)](https://github.com/haskell/haskell-language-server/pull/436) by @pepeiborra
- Add docs on how to choose a formatter
([#432)](https://github.com/haskell/haskell-language-server/pull/432) by @googleson78
- Implement 'Attempt to fill hole' code action
([#431)](https://github.com/haskell/haskell-language-server/pull/431) by @TOTBWF
- Clarify that eval is a lens
([#428)](https://github.com/haskell/haskell-language-server/pull/428) by @Anrock
- Use implicit-hie-cradle-0.2.0.1
([#427)](https://github.com/haskell/haskell-language-server/pull/427) by @jneira
- [retrie] Fix uris in workspace edit
([#424)](https://github.com/haskell/haskell-language-server/pull/424) by @pepeiborra
- Separate paragraphs
([#423)](https://github.com/haskell/haskell-language-server/pull/423) by @jneira
- Include .editorconfig in the contributing section
([#420)](https://github.com/haskell/haskell-language-server/pull/420) by @jneira
- Mention the copy of executables wit ghc version
([#419)](https://github.com/haskell/haskell-language-server/pull/419) by @jneira
- Eval plugin: proper multilined results handling and command-name abbreviations
([#413)](https://github.com/haskell/haskell-language-server/pull/413) by @konn
- Retrie - calculate imports in the command handler
([#408)](https://github.com/haskell/haskell-language-server/pull/408) by @pepeiborra
- Progress reporting for Eval plugin
([#398)](https://github.com/haskell/haskell-language-server/pull/398) by @pepeiborra
- bump ghcide submodule
([#396)](https://github.com/haskell/haskell-language-server/pull/396) by @wz1000
- Fix cradles
([#393)](https://github.com/haskell/haskell-language-server/pull/393) by @pepeiborra
- Case splitting and lambda introduction
([#391)](https://github.com/haskell/haskell-language-server/pull/391) by @isovector
- Use stale data in explicit imports lens
([#383)](https://github.com/haskell/haskell-language-server/pull/383) by @pepeiborra
- Create hls-plugin-api and move plugins to exe
([#379)](https://github.com/haskell/haskell-language-server/pull/379) by @jneira
- Rebase on ghcide HEAD
([#378)](https://github.com/haskell/haskell-language-server/pull/378) by @pepeiborra
- README clarify how exactly to use code evaluation
([#377)](https://github.com/haskell/haskell-language-server/pull/377) by @DunetsNM
- Revise README.md
([#374)](https://github.com/haskell/haskell-language-server/pull/374) by @gihyeonsung

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

```
$ haskell-language-server --probe-tools
haskell-language-server version: 0.3.0.0 (GHC: 8.10.1) (PATH: /Users/luke/.cabal/store/ghc-8.10.1/hskll-lngg-srvr-0.3.0.0-7c6d48c3/bin/haskell-language-server)
Tool versions found on the $PATH
cabal:		3.2.0.0
stack:		2.3.3
ghc:		8.10.2
```

### Pull requests merged

- Bring over https://github.com/pepeiborra/hls-tutorial
([#372](https://github.com/haskell/haskell-language-server/pull/372) by @bubba)
- Update the ghcide upstream to be in haskell/ghcide
([#370](https://github.com/haskell/haskell-language-server/pull/370) by @alanz)
- Add ISSUE_TEMPLATE for github
([#305](https://github.com/haskell/haskell-language-server/pull/305) by @fendor)
- Add use-package to the list of emacs packages
([#343](https://github.com/haskell/haskell-language-server/pull/343) by @rgleichman)
- Implements `:type [+v/+d]` in Eval Plugin
([#361](https://github.com/haskell/haskell-language-server/pull/361) by @konn)
- Bump bounds of hie-bios to 0.7.0
([#357](https://github.com/haskell/haskell-language-server/pull/357) by @maralorn)
- Fix ImportLens plugin to work with GHC 8.10
([#356](https://github.com/haskell/haskell-language-server/pull/356) by @Ailrun)
- Add single file rewrites and ignore unknown files
([#321](https://github.com/haskell/haskell-language-server/pull/321) by @pepeiborra)
- Do not suggest explicit import lists for qualified imports
([#354](https://github.com/haskell/haskell-language-server/pull/354) by @expipiplus1)
- Explicit imports lens (as seen on Twitter)
([#310](https://github.com/haskell/haskell-language-server/pull/310) by @pepeiborra)
- Adds `:kind` and `:kind!` commands to Eval Plugin
([#345](https://github.com/haskell/haskell-language-server/pull/345) by @konn)
- tech(nix): update niv and remove allowbroken
([#350](https://github.com/haskell/haskell-language-server/pull/350) by @willbush)
- Update VS Code Haskell URL/repo
([#338](https://github.com/haskell/haskell-language-server/pull/338) by @Sir4ur0n)
- doc(hack): Add explanation to hack and test HLS
([#329](https://github.com/haskell/haskell-language-server/pull/329) by @Sir4ur0n)
- Apply the module pragmas for evaluation
([#322](https://github.com/haskell/haskell-language-server/pull/322) by @pepeiborra)
- Copy working stack-8.6.5.yaml to stack.yaml
([#332](https://github.com/haskell/haskell-language-server/pull/332) by @jneira)
- tech(nix): Allow broken as retrie is marked as broken
([#331](https://github.com/haskell/haskell-language-server/pull/331) by @Sir4ur0n)
- feat(git): Add install/hie.yaml to gitignore
([#328](https://github.com/haskell/haskell-language-server/pull/328) by @Sir4ur0n)
- Replace wrong occurrences of "engine" by "server"
([#319](https://github.com/haskell/haskell-language-server/pull/319) by @tchoutri)
- Simplify coc.nvim instructions
([#315](https://github.com/haskell/haskell-language-server/pull/315) by @oblitum)
- Coc config file requires a {} nesting everything
([#317](https://github.com/haskell/haskell-language-server/pull/317) by @hyiltiz)
- Restrict opentelemetry version for stack builds
([#312](https://github.com/haskell/haskell-language-server/pull/312) by @jneira)
- Add support for ghc-8.10.2
([#308](https://github.com/haskell/haskell-language-server/pull/308) by @jneira)
- Return nothing if tool is not on the PATH
([#309](https://github.com/haskell/haskell-language-server/pull/309) by @fendor)
- Probe tools cli
([#306](https://github.com/haskell/haskell-language-server/pull/306) by @fendor)
- Add fourmolu plugin (attempt 2) and add Brittany for ghc-8.10.1
([#264](https://github.com/haskell/haskell-language-server/pull/264) by @georgefst)

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
