{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.Completion.Data where

import           Data.Map                                       (Map)
import qualified Data.Map                                       as Map
import qualified Data.Text                                      as T
import           Development.IDE.GHC.Compat.Core                (flagsForCompletion)
import           Distribution.CabalSpecVersion                  (CabalSpecVersion (CabalSpecV2_2),
                                                                 showCabalSpecVersion)
import           Distribution.Pretty                            (prettyShow)
import           Distribution.Types.BuildType                   (knownBuildTypes)
import           Ide.Plugin.Cabal.Completion.Completer.FilePath
import           Ide.Plugin.Cabal.Completion.Completer.Module
import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types    (Completer,
                                                                 ComponentSpec (isRequired, name),
                                                                 defCS, optCS)
import           Ide.Plugin.Cabal.Completion.Types
import           Ide.Plugin.Cabal.LicenseSuggest                (licenseNames)
import           Language.Haskell.Extension
-- | Ad-hoc data type for modelling the available top-level stanzas.
-- Not intended right now for anything else but to avoid string
-- comparisons in 'stanzaKeywordMap' and 'libExecTestBenchCommons'.
data TopLevelStanza
  = Library
  | Executable
  | TestSuite
  | Benchmark
  | ForeignLib
  | Common

-- ----------------------------------------------------------------
-- Completion Data
-- ----------------------------------------------------------------

specToMap :: [(ComponentSpec, b)] -> Map T.Text b
specToMap = Map.fromList . map (\(c, x) -> (name c, x))


specToWeights :: [(ComponentSpec, b)] -> Map T.Text Double
specToWeights = Map.fromList . map (\(c, _) -> (name c, if isRequired c then 1 else 0))

supportedCabalVersions :: [CabalSpecVersion]
supportedCabalVersions = [CabalSpecV2_2 .. maxBound]

-- | Keyword for cabal version; required to be the top line in a cabal file
cabalVersionKeyword :: Map KeyWordName Completer
cabalVersionKeyword = Map.singleton (name $ fst cabalVersionKeywordSpec) (snd cabalVersionKeywordSpec)


cabalVersionKeywordWeight :: Map KeyWordName Double
cabalVersionKeywordWeight = specToWeights [cabalVersionKeywordSpec]

cabalVersionKeywordSpec :: (ComponentSpec, Completer)
cabalVersionKeywordSpec =
  ((optCS "cabal-version:"),
    constantCompleter $
      -- We only suggest cabal versions newer than 2.2
      -- since we don't recommend using older ones.
      map (T.pack . showCabalSpecVersion) supportedCabalVersions)

-- | Top level keywords of a cabal file.
--
-- TODO: we could add descriptions of field values and
-- then show them when inside the field's context
cabalKeywords :: Map KeyWordName Completer
cabalKeywords = specToMap cabalKeywordSpecs


cabalKeywordWeights :: Map T.Text Double
cabalKeywordWeights = specToWeights cabalKeywordSpecs

cabalKeywordSpecs :: [(ComponentSpec, Completer)]
cabalKeywordSpecs =
    [ (defCS "name:", nameCompleter),
      (defCS "version:", noopCompleter),
      (optCS "build-type:", constantCompleter (fmap (T.pack . show) knownBuildTypes)),
      (optCS "license:", weightedConstantCompleter licenseNames weightedLicenseNames),
      (defCS "license-file:", filePathCompleter),
      (defCS "license-files:", filePathCompleter),
      (defCS "copyright:", noopCompleter),
      (defCS "author:", noopCompleter),
      (defCS "maintainer:", noopCompleter), -- email address, use git config?
      (defCS "stability:", noopCompleter),
      (defCS "homepage:", noopCompleter),
      (defCS "bug-reports:", noopCompleter),
      (defCS "package-url:", noopCompleter),
      (defCS "synopsis:", noopCompleter),
      (defCS "description:", noopCompleter),
      (defCS "category:", noopCompleter),
      (defCS "tested-with:", constantCompleter [ "GHC"]),
      (defCS "data-files:", filePathCompleter),
      (optCS "data-dir:", directoryCompleter),
      (defCS "extra-source-files:", filePathCompleter),
      (defCS "extra-doc-files:", filePathCompleter),
      (defCS "extra-tmp-files:", filePathCompleter)
    ]


-- | Map, containing all stanzas in a cabal file as keys,
--  and Maps of their possible nested keywords as values.
stanzaKeywordMapMap :: Map StanzaType (Map KeyWordName Completer)
stanzaKeywordMapMap = specToMap $ map (\(a,b) -> (a, specToMap b)) stanzaKeywordSpec


-- | Map, containing weights for fields nested in stanza fields
stanzaKeywordWeights :: Map StanzaType (Map KeyWordName Double)
stanzaKeywordWeights = Map.map specToWeights stanzaKeywordMap

-- | Map, containing weights for top level stanzas
stanzaWeights :: Map StanzaType Double
stanzaWeights = specToWeights stanzaKeywordSpec

-- | Map, containing all stanzas in a cabal file as keys,
--  and lists of their possible nested specs as values.
stanzaKeywordMap :: Map StanzaType [(ComponentSpec, Completer)]
stanzaKeywordMap = specToMap stanzaKeywordSpec

stanzaKeywordSpec :: [(ComponentSpec, [(ComponentSpec, Completer)])]
stanzaKeywordSpec =
    [ (defCS "library", libraryFieldsSpec <> libExecTestBenchCommonsSpec Library),
      (defCS "executable", executableFieldsSpec <> libExecTestBenchCommonsSpec Executable),
      (defCS "test-suite", testSuiteFieldsSpec <> libExecTestBenchCommonsSpec TestSuite),
      (defCS "benchmark", benchmarkFieldsSpec <> libExecTestBenchCommonsSpec Benchmark),
      (defCS "foreign-library", foreignLibraryFieldsSpec <> libExecTestBenchCommonsSpec ForeignLib),
      (defCS "common", libExecTestBenchCommonsSpec Library),
      (defCS "common", libExecTestBenchCommonsSpec Common),
      (defCS "flag", flagFieldsSpec),
      (defCS "source-repository", sourceRepositoryFieldsSpec),
      (defCS "custom-setup", customSetupFieldsSpec)
    ]

libraryFields :: Map KeyWordName Completer
libraryFields = specToMap libraryFieldsSpec

libraryFieldsWeights :: Map KeyWordName Double
libraryFieldsWeights = specToWeights libraryFieldsSpec

libraryFieldsSpec :: [(ComponentSpec, Completer)]
libraryFieldsSpec =
    [ (defCS "exposed-modules:", modulesCompleter sourceDirsExtractionLibrary),
      (defCS "virtual-modules:", noopCompleter),
      (defCS "exposed:", constantCompleter ["True", "False"]),
      (defCS "visibility:", constantCompleter ["private", "public"]),
      (defCS "reexported-modules:", noopCompleter),
      (defCS "signatures:", noopCompleter),
      (defCS "other-modules:", modulesCompleter sourceDirsExtractionLibrary)
    ]

executableFields :: Map KeyWordName Completer
executableFields = specToMap executableFieldsSpec

executableFieldsWeights :: Map KeyWordName Double
executableFieldsWeights = specToWeights executableFieldsSpec

executableFieldsSpec :: [(ComponentSpec, Completer)]
executableFieldsSpec =
    [ (optCS "main-is:", mainIsCompleter sourceDirsExtractionExecutable),
      (defCS "scope:", constantCompleter ["public", "private"]),
      (defCS "other-modules:", modulesCompleter sourceDirsExtractionExecutable)
    ]

testSuiteFields :: Map KeyWordName Completer
testSuiteFields = specToMap testSuiteFieldsSpec

testSuiteFieldsWeights :: Map KeyWordName Double
testSuiteFieldsWeights = specToWeights testSuiteFieldsSpec


testSuiteFieldsSpec :: [(ComponentSpec, Completer)]
testSuiteFieldsSpec =
    [ (optCS "type:", constantCompleter ["exitcode-stdio-1.0", "detailed-0.9"]),
      (defCS "main-is:", mainIsCompleter sourceDirsExtractionTestSuite),
      (defCS "other-modules:", modulesCompleter sourceDirsExtractionTestSuite)
    ]

benchmarkFields :: Map KeyWordName Completer
benchmarkFields = specToMap benchmarkFieldsSpec


benchmarkFieldsWeights :: Map KeyWordName Double
benchmarkFieldsWeights = specToWeights benchmarkFieldsSpec

benchmarkFieldsSpec :: [(ComponentSpec, Completer)]
benchmarkFieldsSpec =
    [ (defCS "type:", noopCompleter),
      (defCS "main-is:", mainIsCompleter sourceDirsExtractionBenchmark),
      (defCS "other-modules:", modulesCompleter sourceDirsExtractionBenchmark)
    ]

foreignLibraryFields :: Map KeyWordName Completer
foreignLibraryFields = specToMap foreignLibraryFieldsSpec


foreignLibraryFieldsWeights :: Map KeyWordName Double
foreignLibraryFieldsWeights = specToWeights foreignLibraryFieldsSpec

foreignLibraryFieldsSpec :: [(ComponentSpec, Completer)]
foreignLibraryFieldsSpec =
    [ (defCS "type:", constantCompleter ["native-static", "native-shared"]),
      (defCS "options:", constantCompleter ["standalone"]),
      (defCS "mod-def-file:", filePathCompleter),
      (defCS "lib-version-info:", noopCompleter),
      (defCS "lib-version-linux:", noopCompleter)
    ]

sourceRepositoryFields :: Map KeyWordName Completer
sourceRepositoryFields = specToMap sourceRepositoryFieldsSpec

sourceRepositoryFieldsSpec :: [(ComponentSpec, Completer)]
sourceRepositoryFieldsSpec =
    [ (defCS "type:",
        constantCompleter
          [ "darcs",
            "git",
            "svn",
            "cvs",
            "mercurial",
            "hg",
            "bazaar",
            "bzr",
            "arch",
            "monotone"
          ]
      ),
      (defCS "location:", noopCompleter),
      (defCS "module:", noopCompleter),
      (defCS "branch:", noopCompleter),
      (defCS "tag:", noopCompleter),
      (defCS "subdir:", directoryCompleter)
    ]

flagFields :: Map KeyWordName Completer
flagFields = specToMap flagFieldsSpec


flagFieldsWeights :: Map KeyWordName Double
flagFieldsWeights = specToWeights flagFieldsSpec

flagFieldsSpec :: [(ComponentSpec, Completer)]
flagFieldsSpec =
    [ (defCS "description:", noopCompleter),
      (defCS "default:", constantCompleter ["True", "False"]),
      (defCS "manual:", constantCompleter ["False", "True"]),
      (defCS "lib-def-file:", noopCompleter),
      (defCS "lib-version-info:", noopCompleter),
      (defCS "lib-version-linux:", noopCompleter)
    ]

libExecTestBenchCommons :: TopLevelStanza -> Map KeyWordName Completer
libExecTestBenchCommons = specToMap . libExecTestBenchCommonsSpec


libExecTestBenchCommonsWeights :: TopLevelStanza -> Map KeyWordName Double
libExecTestBenchCommonsWeights = specToWeights . libExecTestBenchCommonsSpec

libExecTestBenchCommonsSpec :: TopLevelStanza -> [(ComponentSpec, Completer)]
libExecTestBenchCommonsSpec st =
    [ (defCS "import:", importCompleter),
      (defCS "build-depends:", noopCompleter),
      (defCS "hs-source-dirs:", directoryCompleter),
      (defCS "default-extensions:", constantCompleter $ map (T.pack . prettyShow) allExtensions),
      (defCS "other-extensions:", constantCompleter $ map (T.pack . prettyShow) allExtensions),
      (optCS "default-language:", defaultLanguageCompleter),
      (defCS "other-languages:", noopCompleter),
      (defCS "build-tool-depends:", noopCompleter),
      (defCS "buildable:", constantCompleter ["True", "False"]),
      (defCS "ghc-options:", constantCompleter ghcOptions),
      (defCS "ghc-prof-options:", constantCompleter ghcOptions),
      (defCS "ghc-shared-options:", constantCompleter ghcOptions),
      (defCS "ghcjs-options:", constantCompleter ghcOptions),
      (defCS "ghcjs-prof-options:", constantCompleter ghcOptions),
      (defCS "ghcjs-shared-options:", constantCompleter ghcOptions),
      (defCS "includes:", filePathCompleter),
      (defCS "install-includes:", filePathCompleter),
      (defCS "include-dirs:", directoryCompleter),
      (defCS "autogen-includes:", filePathCompleter),
      (defCS "autogen-modules:", moduleCompleterByTopLevelStanza),
      (defCS "c-sources:", filePathCompleter),
      (defCS "cxx-sources:", filePathCompleter),
      (defCS "asm-sources:", filePathCompleter),
      (defCS "cmm-sources:", filePathCompleter),
      (defCS "js-sources:", filePathCompleter),
      (defCS "extra-libraries:", noopCompleter),
      (defCS "extra-ghci-libraries:", noopCompleter),
      (defCS "extra-bundled-libraries:", noopCompleter),
      (defCS "extra-lib-dirs:", directoryCompleter),
      (defCS "cc-options:", noopCompleter),
      (defCS "cpp-options:", noopCompleter),
      (defCS "cxx-options:", noopCompleter),
      (defCS "cmm-options:", noopCompleter),
      (defCS "asm-options:", noopCompleter),
      (defCS "ld-options:", noopCompleter),
      (defCS "pkgconfig-depends:", noopCompleter),
      (defCS "frameworks:", noopCompleter),
      (defCS "extra-framework-dirs:", directoryCompleter),
      (defCS "mixins:", noopCompleter)
    ]
  where
    --
    moduleCompleterByTopLevelStanza = case st of
      Library -> modulesCompleter sourceDirsExtractionLibrary
      Executable -> modulesCompleter sourceDirsExtractionExecutable
      TestSuite -> modulesCompleter sourceDirsExtractionTestSuite
      Benchmark -> modulesCompleter sourceDirsExtractionBenchmark
      ForeignLib -> modulesCompleter sourceDirsExtractionForeignLib
      Common ->
        -- TODO: We can't provide a module completer because we provide
        -- module completions based on the "hs-source-dirs" after parsing the file,
        -- i.e. based on the 'PackageDescription'.
        -- "common" stanzas are erased in the 'PackageDescription' representation,
        -- thus we can't provide accurate module completers right now, as we don't
        -- know what the 'hs-source-dirs' in the "common" stanza are.
        --
        -- A potential fix would be to introduce an intermediate representation that
        -- parses the '.cabal' file s.t. that we have access to the 'hs-source-dirs',
        -- but not have erased the "common" stanza.
        noopCompleter

customSetupFields :: Map KeyWordName Completer
customSetupFields = specToMap customSetupFieldsSpec


customSetupFieldsWeights :: Map KeyWordName Double
customSetupFieldsWeights = specToWeights customSetupFieldsSpec

customSetupFieldsSpec :: [(ComponentSpec, Completer)]
customSetupFieldsSpec =
    [ (defCS "setup-depends:", noopCompleter)
    , (defCS "build-depends:", noopCompleter)
    , (defCS "build-tools:", noopCompleter)
    , (defCS "default-language:", defaultLanguageCompleter)
    ]

-- | Returns all possible language extensions including disabled ones.
allExtensions :: [Extension]
allExtensions =
  concatMap
    ( \e ->
        -- These pragmas cannot be negated as they are not reversible
        -- by prepending "No".
        if e `notElem` [Unsafe, Trustworthy, Safe]
          then [EnableExtension e, DisableExtension e]
          else [EnableExtension e]
    )
    knownExtensions

-- | Returns all possible default languages
defaultLanguageCompleter :: Completer
defaultLanguageCompleter = constantCompleter $ map (T.pack . prettyShow) knownLanguages

-- | Contains a map of the most commonly used licenses, weighted by their popularity.
--
--  The data was extracted by Kleidukos from the alternative hackage frontend flora.pm.
weightedLicenseNames :: Map T.Text Double
weightedLicenseNames =
  fmap statisticsToWeight $
    Map.fromList
      [ ("BSD-3-Clause", 9955),
        ("MIT", 3336),
        ("GPL-3.0-only", 679),
        ("LicenseRef-OtherLicense", 521),
        ("Apache-2.0", 514),
        ("LicenseRef-GPL", 443),
        ("LicenseRef-PublicDomain", 318),
        ("MPL-2.0", 288),
        ("BSD-2-Clause", 174),
        ("GPL-2.0-only", 160),
        ("LicenseRef-LGPL", 146),
        ("LGPL-2.1-only", 112),
        ("LGPL-3.0-only", 100),
        ("AGPL-3.0-only", 96),
        ("ISC", 89),
        ("LicenseRef-Apache", 45),
        ("GPL-3.0-or-later", 43),
        ("BSD-2-Clause-Patent", 33),
        ("GPL-2.0-or-later", 21),
        ("CC0-1.0", 16),
        ("AGPL-3.0-or-later", 15),
        ("LGPL-2.1-or-later", 12),
        ("(BSD-2-Clause OR Apache-2.0)", 10),
        ("(Apache-2.0 OR MPL-2.0)", 8),
        ("LicenseRef-AGPL", 6),
        ("(BSD-3-Clause OR Apache-2.0)", 4),
        ("0BSD", 3),
        ("BSD-4-Clause", 3),
        ("LGPL-3.0-or-later", 3),
        ("LicenseRef-LGPL-2", 2),
        ("GPL-2.0-or-later AND BSD-3-Clause", 2),
        ("NONE", 2),
        ("Zlib", 2),
        ("(Apache-2.0 OR BSD-3-Clause)", 2),
        ("BSD-3-Clause AND GPL-2.0-or-later", 2),
        ("BSD-3-Clause AND GPL-3.0-or-later", 2)
      ]
  where
    -- Add weights to each usage value from above, the weights are chosen
    -- arbitrarily in order for completions to prioritize which licenses to
    -- suggest in a sensible way
    statisticsToWeight :: Int -> Double
    statisticsToWeight stat
      | stat < 10 = 0.1
      | stat < 20 = 0.3
      | stat < 50 = 0.4
      | stat < 100 = 0.5
      | stat < 500 = 0.6
      | stat < 650 = 0.7
      | otherwise = 0.9

ghcOptions :: [T.Text]
ghcOptions = map T.pack $ flagsForCompletion False

