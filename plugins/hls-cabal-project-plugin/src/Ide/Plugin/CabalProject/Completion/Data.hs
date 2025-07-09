{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalProject.Completion.Data where

import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import qualified Data.Text                                    as T
import           Development.IDE.GHC.Compat.Core              (flagsForCompletion)
import           Distribution.CabalSpecVersion                (CabalSpecVersion (CabalSpecV2_2),
                                                               showCabalSpecVersion)
-- import           Ide.Plugin.Cabal.Completion.Completer.FilePath
-- import           Ide.Plugin.Cabal.Completion.Completer.Module
-- import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types  (Completer)
import           Ide.Plugin.Cabal.Completion.Types
-- import           Ide.Plugin.Cabal.LicenseSuggest                (licenseNames)

-- ----------------------------------------------------------------
-- Completion Data
-- ----------------------------------------------------------------

-- supportedCabalVersions :: [CabalSpecVersion]
-- supportedCabalVersions = [CabalSpecV2_2 .. maxBound]

-- -- | Keyword for cabal version; required to be the top line in a cabal file
-- cabalVersionKeyword :: Map KeyWordName Completer
-- cabalVersionKeyword =
--   Map.singleton "cabal-version:" $
--     constantCompleter $
--       -- We only suggest cabal versions newer than 2.2
--       -- since we don't recommend using older ones.
--       map (T.pack . showCabalSpecVersion) supportedCabalVersions

-- | Top level keywords of a cabal file.
--
-- TODO: we could add descriptions of field values and
-- then show them when inside the field's context
cabalProjectKeywords :: Map KeyWordName Completer
cabalProjectKeywords =
  Map.fromList
    [ -- projectConfigFieldGrammar
      ("packages:", noopCompleter),
      ("optional-packages:", noopCompleter),
      ("extra-packages:", noopCompleter),
      -- projectConfigBuildOnlyFieldGrammar
      ("verbose:", constantCompleter ["0", "1", "2", "3"]), -- not sure if this works/makes sense?
      ("build-summary:", noopCompleter),
      ("build-log:", noopCompleter),
      ("remote-build-reporting:", noopCompleter),
      ("report-planning-failure:", noopCompleter),
      ("symlink-bindir:", noopCompleter),
      ("jobs:", noopCompleter),
      ("semaphore:", noopCompleter),
      ("keep-going:", noopCompleter),
      ("offline:", noopCompleter),
      ("haddock-keep-temp-files:", noopCompleter),
      ("http-transport:", noopCompleter),
      ("ignore-expiry:", noopCompleter),
      ("remote-repo-cache:", noopCompleter),
      ("logs-dir:", noopCompleter),
      -- projectConfigSharedFieldGrammar
      ("builddir:", noopCompleter),
      ("project-dir:", noopCompleter),
      ("project-file:", noopCompleter),
      ("ignore-project:", noopCompleter),
      ("compiler:", noopCompleter),
      ("with-compiler:", noopCompleter),
      ("with-hc-pkg:", noopCompleter),
      ("doc-index-file:", noopCompleter),
      ("package-dbs:", noopCompleter),
      ("active-repositories:", noopCompleter),
      ("index-state:", noopCompleter),
      ("store-dir:", noopCompleter),
      ("constraints:", noopCompleter),
      ("preferences:", noopCompleter),
      ("cabal-lib-version:", noopCompleter),
      ("solver:", noopCompleter),
      ("allow-older:", noopCompleter),
      ("allow-newer:", noopCompleter),
      ("write-ghc-environment-files:", noopCompleter),
      ("max-backjumps:", noopCompleter),
      ("reorder-goals:", noopCompleter),
      ("count-conflicts:", noopCompleter),
      ("fine-grained-conflicts:", noopCompleter),
      ("minimize-conflict-set:", noopCompleter),
      ("strong-flags:", noopCompleter),
      ("allow-boot-library-installs:", noopCompleter),
      ("reject-unconstrained-dependencies:", noopCompleter),
      ("per-component:", noopCompleter),
      ("independent-goals:", noopCompleter),
      ("prefer-oldest:", noopCompleter),
      ("extra-prog-path-shared-only:", noopCompleter),
      ("multi-repl:", noopCompleter)
    ]

packageFields :: Map KeyWordName Completer
packageFields =
  Map.fromList
    [ -- packageConfigFieldGrammar
      ("haddock-all:", noopCompleter),
      ("extra-prog-path:", noopCompleter),
      ("flags:", noopCompleter),
      ("library-vanilla:", noopCompleter),
      ("shared:", noopCompleter),
      ("static:", noopCompleter),
      ("exectable-dynamic:", noopCompleter),
      ("executable-static:", noopCompleter),
      ("profiling:", noopCompleter),
      ("library-profiling:", noopCompleter),
      ("profiling-shared:", noopCompleter),
      ("exectable-profiling:", noopCompleter),
      ("profiling-detail:", noopCompleter),
      ("library-profiling-detail:", noopCompleter),
      ("configure-options:", noopCompleter),
      ("optimization:", noopCompleter),
      ("program-prefix:", noopCompleter),
      ("program-suffix:", noopCompleter),
      ("extra-lib-dirs:", noopCompleter),
      ("extra-lib-dirs-static:", noopCompleter),
      ("extra-framework-dirs:", noopCompleter),
      ("extra-include-dirs:", noopCompleter),
      ("library-for-ghci:", noopCompleter),
      ("split-sections:", noopCompleter),
      ("split-objs:", noopCompleter),
      ("executable-stripping:", noopCompleter),
      ("library-stripping:", noopCompleter),
      ("tests:", noopCompleter),
      ("benchmarks:", noopCompleter),
      ("relocatable:", noopCompleter),
      ("debug-info:", noopCompleter),
      ("build-info:", noopCompleter),
      ("run-tests:", noopCompleter),
      ("documentation:", noopCompleter),
      ("haddock-hoogle:", noopCompleter),
      ("haddock-html:", noopCompleter),
      ("haddock-html-location:", noopCompleter),
      ("haddock-foreign-libraries:", noopCompleter),
      ("haddock-executables:", noopCompleter),
      ("haddock-tests:", noopCompleter),
      ("haddock-benchmarks:", noopCompleter),
      ("haddock-internal:", noopCompleter),
      ("haddock-css:", noopCompleter),
      ("haddock-hyperlink-source:", noopCompleter),
      ("haddock-quickjump:", noopCompleter),
      ("haddock-hscolour-css:", noopCompleter),
      ("haddock-contents-location:", noopCompleter),
      ("haddock-index-location:", noopCompleter),
      ("haddock-base-url:", noopCompleter),
      ("haddock-resources-dir:", noopCompleter),
      ("haddock-output-dir:", noopCompleter),
      ("haddock-use-unicode:", noopCompleter),
      ("haddock-for-hackage:", noopCompleter),
      ("test-log:", noopCompleter),
      ("test-machine-log:", noopCompleter),
      ("test-show-details:", noopCompleter),
      ("test-keep-tix-files:", noopCompleter),
      ("test-wrapper:", noopCompleter),
      ("test-fail-when-no-test-suites:", noopCompleter),
      ("test-options:", noopCompleter),
      ("benchmark-options:", noopCompleter),
      -- packageConfigCoverageGrammar
      ("coverage:", noopCompleter)
    ]

-- | Map, containing all stanzas in a cabal file as keys,
--  and lists of their possible nested keywords as values.
stanzaKeywordMap :: Map StanzaType (Map KeyWordName Completer)
stanzaKeywordMap =
  Map.fromList
    [ ("package", packageFields)
    ]
