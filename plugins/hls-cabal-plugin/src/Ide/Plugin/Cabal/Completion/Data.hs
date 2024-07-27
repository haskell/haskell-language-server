{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Ide.Plugin.Cabal.Completion.Data where

import           Data.Map                                       (Map)
import qualified Data.Map                                       as Map
import qualified Data.Text                                      as T
import           Development.IDE.GHC.Compat.Core                (flagsForCompletion)
import           Distribution.CabalSpecVersion                  (CabalSpecVersion (CabalSpecV2_2),
                                                                 showCabalSpecVersion)
import           Ide.Plugin.Cabal.Completion.Completer.FilePath
import           Ide.Plugin.Cabal.Completion.Completer.Module
import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types    (Completer)
import           Ide.Plugin.Cabal.Completion.Types
import           Ide.Plugin.Cabal.LicenseSuggest                (licenseNames)

-- ----------------------------------------------------------------
-- Completion Data
-- ----------------------------------------------------------------

-- | Keyword for cabal version; required to be the top line in a cabal file
cabalVersionKeyword :: Map KeyWordName Completer
cabalVersionKeyword =
  Map.singleton "cabal-version:" $
    constantCompleter $
      -- We only suggest cabal versions newer than 2.2
      -- since we don't recommend using older ones.
      map (T.pack . showCabalSpecVersion) [CabalSpecV2_2 .. maxBound]

-- | Top level keywords of a cabal file.
--
-- TODO: we could add descriptions of field values and
-- then show them when inside the field's context
cabalKeywords :: Map KeyWordName Completer
cabalKeywords =
  Map.fromList
    [ ("name:", nameCompleter),
      ("version:", noopCompleter),
      ("build-type:", constantCompleter ["Simple", "Custom", "Configure", "Make"]),
      ("license:", weightedConstantCompleter licenseNames weightedLicenseNames),
      ("license-file:", filePathCompleter),
      ("license-files:", filePathCompleter),
      ("copyright:", noopCompleter),
      ("author:", noopCompleter),
      ("maintainer:", noopCompleter), -- email address, use git config?
      ("stability:", noopCompleter),
      ("homepage:", noopCompleter),
      ("bug-reports:", noopCompleter),
      ("package-url:", noopCompleter),
      ("synopsis:", noopCompleter),
      ("description:", noopCompleter),
      ("category:", noopCompleter),
      ("tested-with:", constantCompleter ["GHC"]),
      ("data-files:", filePathCompleter),
      ("data-dir:", directoryCompleter),
      ("extra-source-files:", filePathCompleter),
      ("extra-doc-files:", filePathCompleter),
      ("extra-tmp-files:", filePathCompleter)
    ]

-- | Map, containing all stanzas in a cabal file as keys,
--  and lists of their possible nested keywords as values.
stanzaKeywordMap :: Map StanzaType (Map KeyWordName Completer)
stanzaKeywordMap =
  Map.fromList
    [ ("library", libraryFields <> libExecTestBenchCommons),
      ("executable", executableFields <> libExecTestBenchCommons),
      ("test-suite", testSuiteFields <> libExecTestBenchCommons),
      ("benchmark", benchmarkFields <> libExecTestBenchCommons),
      ("foreign-library", foreignLibraryFields <> libExecTestBenchCommons),
      ("common", libExecTestBenchCommons),
      ("flag", flagFields),
      ("source-repository", sourceRepositoryFields)
    ]

libraryFields :: Map KeyWordName Completer
libraryFields =
  Map.fromList
    [ ("exposed-modules:", modulesCompleter sourceDirsExtractionLibrary),
      ("virtual-modules:", noopCompleter),
      ("exposed:", constantCompleter ["True", "False"]),
      ("visibility:", constantCompleter ["private", "public"]),
      ("reexported-modules:", noopCompleter),
      ("signatures:", noopCompleter),
      ("other-modules:", modulesCompleter sourceDirsExtractionLibrary)
    ]

executableFields :: Map KeyWordName Completer
executableFields =
  Map.fromList
    [ ("main-is:", mainIsCompleter sourceDirsExtractionExecutable),
      ("scope:", constantCompleter ["public", "private"]),
      ("other-modules:", modulesCompleter sourceDirsExtractionExecutable)
    ]

testSuiteFields :: Map KeyWordName Completer
testSuiteFields =
  Map.fromList
    [ ("type:", constantCompleter ["exitcode-stdio-1.0", "detailed-0.9"]),
      ("main-is:", mainIsCompleter sourceDirsExtractionTestSuite),
      ("other-modules:", modulesCompleter sourceDirsExtractionTestSuite)
    ]

benchmarkFields :: Map KeyWordName Completer
benchmarkFields =
  Map.fromList
    [ ("type:", noopCompleter),
      ("main-is:", mainIsCompleter sourceDirsExtractionBenchmark),
      ("other-modules:", modulesCompleter sourceDirsExtractionBenchmark)
    ]

foreignLibraryFields :: Map KeyWordName Completer
foreignLibraryFields =
  Map.fromList
    [ ("type:", constantCompleter ["native-static", "native-shared"]),
      ("options:", constantCompleter ["standalone"]),
      ("mod-def-file:", filePathCompleter),
      ("lib-version-info:", noopCompleter),
      ("lib-version-linux:", noopCompleter)
    ]

sourceRepositoryFields :: Map KeyWordName Completer
sourceRepositoryFields =
  Map.fromList
    [ ( "type:",
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
      ("location:", noopCompleter),
      ("module:", noopCompleter),
      ("branch:", noopCompleter),
      ("tag:", noopCompleter),
      ("subdir:", directoryCompleter)
    ]

flagFields :: Map KeyWordName Completer
flagFields =
  Map.fromList
    [ ("description:", noopCompleter),
      ("default:", constantCompleter ["True", "False"]),
      ("manual:", constantCompleter ["False", "True"]),
      ("lib-def-file:", noopCompleter),
      ("lib-version-info:", noopCompleter),
      ("lib-version-linux:", noopCompleter)
    ]

libExecTestBenchCommons :: Map KeyWordName Completer
libExecTestBenchCommons =
  Map.fromList
    [ ("import:", importCompleter),
      ("build-depends:", noopCompleter),
      ("hs-source-dirs:", directoryCompleter),
      ("default-extensions:", noopCompleter),
      ("other-extensions:", noopCompleter),
      ("default-language:", constantCompleter ["GHC2021", "Haskell2010", "Haskell98"]),
      ("other-languages:", noopCompleter),
      ("build-tool-depends:", noopCompleter),
      ("buildable:", constantCompleter ["True", "False"]),
      ("ghc-options:", constantCompleter ghcOptions),
      ("ghc-prof-options:", constantCompleter ghcOptions),
      ("ghc-shared-options:", constantCompleter ghcOptions),
      ("ghcjs-options:", constantCompleter ghcOptions),
      ("ghcjs-prof-options:", constantCompleter ghcOptions),
      ("ghcjs-shared-options:", constantCompleter ghcOptions),
      ("includes:", filePathCompleter),
      ("install-includes:", filePathCompleter),
      ("include-dirs:", directoryCompleter),
      ("c-sources:", filePathCompleter),
      ("cxx-sources:", filePathCompleter),
      ("asm-sources:", filePathCompleter),
      ("cmm-sources:", filePathCompleter),
      ("js-sources:", filePathCompleter),
      ("extra-libraries:", noopCompleter),
      ("extra-ghci-libraries:", noopCompleter),
      ("extra-bundled-libraries:", noopCompleter),
      ("extra-lib-dirs:", directoryCompleter),
      ("cc-options:", noopCompleter),
      ("cpp-options:", noopCompleter),
      ("cxx-options:", noopCompleter),
      ("cmm-options:", noopCompleter),
      ("asm-options:", noopCompleter),
      ("ld-options:", noopCompleter),
      ("pkgconfig-depends:", noopCompleter),
      ("frameworks:", noopCompleter),
      ("extra-framework-dirs:", directoryCompleter),
      ("mixins:", noopCompleter)
    ]

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
