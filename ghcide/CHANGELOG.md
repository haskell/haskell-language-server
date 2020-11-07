### 0.5.0 (2020-10-08)
* Use implicit-hie-0.1.2.0 (#880) - (Javier Neira)
* Clarify and downgrade implicit-hie message (#883) - (Avi Dessauer)
* Switch back to bytecode (#873) - (wz1000)
* Add code action for remove all redundant imports (#867) - (Potato Hatsue)
* Fix pretty printer for diagnostic ranges (#871) - (Martin Huschenbett)
* Canonicalize import dirs (#870) - (Pepe Iborra)
* Do not show internal hole names (#852) - (Alejandro Serrano)
* Downgrade file watch debug log to logDebug from logInfo (#848) - (Matthew Pickering)
* Pull in local bindings (#845) - (Sandy Maguire)
* Use object code for Template Haskell, emit desugarer warnings (#836) - (wz1000)
* Fix code action for adding missing constraints to type signatures (#839) - (Jan Hrcek)
* Fix duplicated completions (#837) - (Vitalii)
* FileExists: set one watcher instead of thousands (#831) - (Michael Peyton Jones)
* Drop 8.4 support (#834) - (wz1000)
* Add GetHieAsts rule, Replace SpanInfo, add support for DocumentHighlight and scope-aware completions for local variables (#784) - (wz1000)
* Tag unused warning as such (#815) - (Alejandro Serrano)
* Update instructions for stty error in windows (#825) - (Javier Neira)
* Fix docs tooltip for base libraries on Windows (#814) - (Nick Dunets)
* Fix documentation (or source) link when html file is less specific than module (#766) - (Nick Dunets)
* Add completion tests for records. (#804) - (Guru Devanla)
* Restore identifiers missing from hi file (#741) - (maralorn)
* Fix import suggestions when dot is typed (#800) - (Marcelo Lazaroni)

### 0.4.0 (2020-09-15)
* Fixes for GHC source plugins: dotpreprocessor works now - (srid)
* Use implicit-hie when no explicit hie.yaml (#782) - (Javier Neira)
* Extend position mapping with fuzzy ranges (#785) - (wz1000)
* Sort import suggestions (#793) - (Pepe Iborra)
* Save source files with HIE files (#701) - (fendor)
* Fully asynchronous request handling (#767) - (Pepe Iborra)
* Refinement holes (#748) - (Pepe Iborra)
* Fix haddock to markdown conversion (#757) - (George Thomas)
* Expose `getCompletionsLSP` to allow completions in hls (#756) - (wz1000)
* Suggestions for missing imports from local modules (#739) - (Pepe Iborra)
* Dynamically load libm on Linux for each new session (#723) - (Luke Lau)
* Use InitializeParams.rootUri for initial session setup (#713) - (shaurya gupta)
* Show documentation on hover for symbols defined in the same module (#691) - (wz1000)
* Suggest open imports (#740) - (Pepe Iborra)
* module Development.IDE (#724) - (Pepe Iborra)
* Ignore -Werror (#738) - (Pepe Iborra)
* Fix issue #710: fix suggest delete binding  (#728) - (Ray Shih)
* Generate doc file URL via LSP (to fix it for Windows) (#721) - (Nick Dunets)
* Fix `.hie` file location for `.hs-boot` files (#690) - (wz1000)
* Use argsVerbose to determine log level in test mode (#717) - (Ziyang Liu)
* output which cradle files were found (#716) - (Adam Sandberg Eriksson)
* Typecheck entire project on Initial Load and typecheck reverse dependencies of a file on saving (#688) - (wz1000)

### 0.3.0 (2020-09-02)

* CI: remove (internal) DA Slack notifications (#750) - (Gary Verhaegen)
* Add session-loader to hie.yaml (#714) - (Luke Lau)
* Codeaction for exporting unused top-level bindings (#711) - (shaurya gupta)
* Add links to haddock and hscolour pages in documentation (#699) - (Luke Lau)
* Expose GHC.Compat module (#709) - (Pepe Iborra)
* Move session loading logic into ghcide library (#697) - (Luke Lau)
* Code action: remove redundant constraints for type signature (#692) - (Denis Frezzato)
* Fix Binary instance of Q to handle empty file paths (#707) - (Moritz Kiefer)
* Populate ms_hs_date in GetModSummary rule (#694) - (Pepe Iborra)
* Allow GHC plugins to be called with an updated StringBuffer (#698) - (Alfredo Di Napoli)
* Relax upper bounds for GHC 8.10.1 (#705) - (Pepe Iborra)
* Obtain the GHC libdir at runtime (#696) - (Luke Lau)
* Expect bench experiments to fail with Cabal (#704) - (Pepe Iborra)
* Bump lodash from 4.17.15 to 4.17.19 in /extension (#702) - (dependabot[bot])
* Update to hie-bios 0.6.1 (#693) - (fendor)
* Backport HIE files to GHC 8.6 (#689) - (wz1000)
* Performance improvements for GetSpanInfo (#681) - (Pepe Iborra)
* Code action add default type annotation to remove `-Wtype-defaults` warning (#680) - (Serhii)
* Use a global namecache to read `.hie` files (#677) - (wz1000)
* Completions need not depend on typecheck of the current file (#670) - (Pepe Iborra)
* Fix spaninfo Haddocks for local modules (#678) - (Pepe Iborra)
* Avoid excessive retypechecking of TH codebases (#673) - (Pepe Iborra)
* Use stale information if it's available to answer requests quickly (#624) - (Matthew Pickering)
* Code action: add constraint (#653) - (Denis Frezzato)
* Make BenchHist non buildable by default and save logs (#666) - (Pepe Iborra)
* Delete unused top level binding code action (#657) - (Serhii)
* stack810.yaml: bump (#651) - (Domen Kozar)
* Fix debouncer for 0 delay (#662) - (Pepe Iborra)
* Interface file fixes (#645) - (Pepe Iborra)
* Retry GHC 8.10 on Windows (#661) - (Moritz Kiefer)
* Finer dependencies for GhcSessionFun (#643) - (Pepe Iborra)
* Send WorkDoneProgressEnd only when work is done (#649) - (Pepe Iborra)
* Add a note on differential benchmarks (#647) - (Pepe Iborra)
* Cache a ghc session per file of interest (#630) - (Pepe Iborra)
* Remove `Strict` from the language extensions used for code actions (#638) - (Torsten Schmits)
* Report progress when setting up cradle (#644) - (Luke Lau)
* Fix crash when writing to a Barrier more than once (#637) - (Pepe Iborra)
* Write a cabal.project file in the benchmark example (#640) - (Pepe Iborra)
* Performance analysis over time (#629) - (Pepe Iborra)
* More benchmarks (#625) - (Pepe Iborra)
* Canonicalize the locations in the cradle tests (#628) - (Luke Lau)
* Add hie.yaml.stack and use none cradle for test data (#626) - (Javier Neira)
* Fix a bug in getHiFileRule (#623) - (Pepe Iborra)
* ghc initialization error handling (#609) - (Pepe Iborra)
* Fix regression in getSpanInfoRule (#622) - (Pepe Iborra)
* Restore Shake profiling (#621) - (Pepe Iborra)
* Use a better noRange (#612) - (Neil Mitchell)
* Add back a .ghci file (#607) - (Neil Mitchell)
* #573, make haddock errors warnings with the word Haddock in front (#608) - (Neil Mitchell)
* Implement Goto Type Definition (#533) - (Matthew Pickering)
* remove unnecessary FileExists dependency in GetHiFile (#589) - (Pepe Iborra)
* ShakeSession and shakeEnqueue (#554) - (Pepe Iborra)
* Benchmark suite (#590) - (Pepe Iborra)

### 0.2.0 (2020-06-02)

* Multi-component support (thanks @mpickering)
* Support for GHC 8.10 (thanks @sheaf and @chshersh)
* Fix some TH issues (thanks @mpickering)
* Automatically pick up changes to cradle dependencies (e.g. cabal
  files) (thanks @jinwoo)
* Track dependencies when using `qAddDependentFile` (thanks @mpickering)
* Add record fields to document symbols outline (thanks @bubba)
* Fix some space leaks (thanks @mpickering)
* Strip redundant path information from diagnostics (thanks @tek)
* Fix import suggestions for operators (thanks @eddiemundo)
* Significant reductions in memory usage by using interfaces and `.hie` files (thanks
  @pepeiborra)
* Minor improvements to completions
* More comprehensive suggestions for missing imports (thanks @pepeiborra)
* Group imports in document outline (thanks @fendor)
* Upgrade to haskell-lsp-0.22 (thanks @bubba)
* Upgrade to hie-bios 0.5 (thanks @fendor)

### 0.1.0 (2020-02-04)

* Code action for inserting new definitions (see #309).
* Better default GC settings (see #329 and #333).
* Various performance improvements (see #322 and #384).
* Improvements to hover information (see #317 and #338).
* Support GHC 8.8.2 (see #355).
* Include keywords in completions (see #351).
* Fix some issues with aborted requests (see #353).
* Use hie-bios 0.4.0 (see #382).
* Avoid stuck progress reporting (see #400).
* Only show progress notifications after 0.1s (see #392).
* Progress reporting is now in terms of the number of files rather
  than the number of shake rules (see #379).

### 0.0.6 (2020-01-10)

* Fix type in hover information for do-notation and list
  comprehensions (see #243).
* Fix hover and goto-definition for multi-clause definitions (see #252).
* Upgrade to `hie-bios-0.3` (see #257)
* Upgrade to `haskell-lsp-0.19` (see #254)
* Code lenses for missing signatures are displayed even if the warning
  has not been enabled. The warning itself will not be shown if it is
  not enabled. (see #232)
* Define `__GHCIDE__` when running CPP to allow for `ghcide`-specific
  workarounds. (see #264)
* Fix some filepath normalization issues. (see #266)
* Fix build with `shake-0.18.4` (see #272)
* Fix hover for type constructors and type classes. (see #267)
* Support custom preprocessors (see #282)
* Add support for code completions (see #227)
* Code action for removing redundant symbols from imports (see #290)
* Support document symbol requests (see #293)
* Show CPP errors as diagnostics (see #296)
* Code action for adding suggested imports (see #295)

### 0.0.5 (2019-12-12)

* Support for GHC plugins (see #192)
* Update to haskell-lsp 0.18 (see #203)
* Initial support for `TemplateHaskell` (see #222)
* Code lenses for missing signatures. These are only shown if
  `-Wmissing-signatures` is enabled. (see #224)
* Fix path normalisation on Windows (see #225)
* Fix flickering of the progress indicator (see #230)

### 0.0.4 (2019-10-20)

* Add a ``--version`` cli option (thanks @jacg)
* Update to use progress reporting as defined in LSP 3.15. The VSCode
  extension has also been updated and should now be making use of
  this.
* Properly declare that we should support code actions. This helps
  with some clients that rely on this information to enable code
  actions (thanks @jacg).
* Fix a race condition caused by sharing the finder cache between
  concurrent compilations.
* Avoid normalizing include dirs. This avoids issues where the same
  file ends up twice in the module graph, e.g., with different casing
  for drive letters.

### 0.0.3 (2019-09-21)
