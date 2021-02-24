# Changelog for haskell-language-server

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
-  Doctest comment parsing using module annotations in Eval Plugin
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
$ ghcup install hls
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
