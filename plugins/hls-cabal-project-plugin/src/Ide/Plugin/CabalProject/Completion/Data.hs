{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalProject.Completion.Data where

import           Data.Map                                       (Map)
import qualified Data.Map                                       as Map
import qualified Data.Text                                      as T
import           Development.IDE.GHC.Compat.Core                (flagsForCompletion)
import           Distribution.CabalSpecVersion                  (CabalSpecVersion (CabalSpecV2_2),
                                                                 showCabalSpecVersion)
import           Ide.Plugin.Cabal.Completion.Completer.FilePath (directoryCompleter,
                                                                 filePathCompleter)
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types    (Completer)
import           Ide.Plugin.Cabal.Completion.Types

-- | Ad-hoc data type for modelling the available top-level stanzas.
-- Not intended right now for anything else but to avoid string
-- comparisons in 'stanzaKeywordMap'.
data TopLevelStanza
  = Package
  | ProgramOptions

-- ----------------------------------------------------------------
-- Completion Data
-- ----------------------------------------------------------------

-- | Top level keywords of a cabal.project file.
--
-- TODO: we could add descriptions of field values and
-- then show them when inside the field's context
cabalProjectKeywords :: Map KeyWordName Completer
cabalProjectKeywords =
  Map.fromList
    [ ("packages:", filePathCompleter),
      ("optional-packages:", filePathCompleter),
      ("extra-packages:", filePathCompleter),
      ("verbose:", constantCompleter ["0", "1", "2", "3"]),
      ("build-summary:", filePathCompleter),
      ("build-log:", noopCompleter),
      ("remote-build-reporting:", noopCompleter),
      ("report-planning-failure:", noopCompleter),
      ("symlink-bindir:", noopCompleter),
      ("jobs:", noopCompleter),
      ("semaphore:", noopCompleter),
      ("keep-going:", constantCompleter ["False", "True"]),
      ("offline:", noopCompleter),
      ("haddock-keep-temp-files:", constantCompleter ["False", "True"]),
      ("http-transport:", constantCompleter ["curl", "wget", "powershell", "plain-http"]),
      ("ignore-expiry:", constantCompleter ["False", "True"]),
      ("remote-repo-cache:", noopCompleter),
      ("logs-dir:", noopCompleter),
      ("builddir:", noopCompleter),
      ("project-dir:", noopCompleter),
      ("project-file:", noopCompleter),
      ("ignore-project:", noopCompleter),
      ("compiler:", constantCompleter ["ghc", "ghcjs", "jhc", "lhc", "uhc", "haskell-suite"]),
      ("with-compiler:", filePathCompleter),
      ("with-hc-pkg:", filePathCompleter),
      ("doc-index-file:", noopCompleter),
      ("package-dbs:", noopCompleter),
      ("active-repositories:", noopCompleter),
      ("index-state:", noopCompleter),
      ("store-dir:", noopCompleter),
      ("constraints:", noopCompleter),
      ("preferences:", noopCompleter),
      ("cabal-lib-version:", noopCompleter),
      ("solver:", constantCompleter ["modular"]),
      ("allow-older:", noopCompleter),
      ("allow-newer:", noopCompleter),
      ("write-ghc-environment-files:", constantCompleter ["never", "always", "ghc8.4.4+"]),
      ("max-backjumps:", noopCompleter),
      ("reorder-goals:", constantCompleter ["False", "True"]),
      ("count-conflicts:", constantCompleter ["True", "False"]),
      ("fine-grained-conflicts:", constantCompleter ["True", "False"]),
      ("minimize-conflict-set:", constantCompleter ["False", "True"]),
      ("strong-flags:", constantCompleter ["False", "True"]),
      ("allow-boot-library-installs:", constantCompleter ["False", "True"]),
      ("reject-unconstrained-dependencies:", constantCompleter ["none", "all"]),
      ("per-component:", noopCompleter),
      ("independent-goals:", noopCompleter),
      ("prefer-oldest:", noopCompleter),
      ("extra-prog-path-shared-only:", noopCompleter),
      ("multi-repl:", noopCompleter),
      ("benchmarks:", constantCompleter ["False", "True"]),
      ("import:", filePathCompleter)
    ]

packageFields :: Map KeyWordName Completer
packageFields =
  Map.fromList
    [ ("haddock-all:", constantCompleter ["False", "True"]),
      ("extra-prog-path:", filePathCompleter),
      ("flags:", noopCompleter),
      ("library-vanilla:", constantCompleter ["True", "False"]),
      ("shared:", constantCompleter ["False", "True"]),
      ("static:", constantCompleter ["False", "True"]),
      ("exectable-dynamic:", constantCompleter ["False", "True"]),
      ("executable-static:", constantCompleter ["False", "True"]),
      ("profiling:", constantCompleter ["False", "True"]),
      ("library-profiling:", constantCompleter ["False", "True"]),
      ("profiling-shared:", noopCompleter),
      ("exectable-profiling:", constantCompleter ["False", "True"]),
      ("profiling-detail:", constantCompleter ["default", "none", "exported-functions", "toplevel-functions", "all-functions"]),
      ("library-profiling-detail:", constantCompleter ["default", "none", "exported-functions", "toplevel-functions", "all-functions"]),
      ("configure-options:", noopCompleter),
      ("optimization:", constantCompleter ["0", "1", "2", "True", "False"]),
      ("program-prefix:", noopCompleter),
      ("program-suffix:", noopCompleter),
      ("extra-lib-dirs:", directoryCompleter),
      ("extra-lib-dirs-static:", directoryCompleter),
      ("extra-framework-dirs:", directoryCompleter),
      ("extra-include-dirs:", directoryCompleter),
      ("library-for-ghci:", constantCompleter ["True", "False"]),
      ("split-sections:", constantCompleter ["False", "True"]),
      ("split-objs:", constantCompleter ["False", "True"]),
      ("executable-stripping:", constantCompleter ["True", "False"]),
      ("library-stripping:", constantCompleter ["False", "True"]),
      ("tests:", constantCompleter ["False", "True"]),
      ("benchmarks:", constantCompleter ["False", "True"]),
      ("relocatable:", constantCompleter ["False", "True"]),
      ("debug-info:", noopCompleter),
      ("build-info:", noopCompleter),
      ("run-tests:", constantCompleter ["False", "True"]),
      ("documentation:", constantCompleter ["False", "True"]),
      ("haddock-hoogle:", constantCompleter ["False", "True"]),
      ("haddock-html:", constantCompleter ["True", "False"]),
      ("haddock-html-location:", noopCompleter),
      ("haddock-foreign-libraries:", noopCompleter),
      ("haddock-executables:", constantCompleter ["False", "True"]),
      ("haddock-tests:", constantCompleter ["False", "True"]),
      ("haddock-benchmarks:", constantCompleter ["False", "True"]),
      ("haddock-internal:", constantCompleter ["False", "True"]),
      ("haddock-css:", filePathCompleter),
      ("haddock-hyperlink-source:", constantCompleter ["False", "True"]),
      ("haddock-quickjump:", noopCompleter),
      ("haddock-hscolour-css:", filePathCompleter),
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
      ("coverage:", constantCompleter ["False", "True"]),
      ("ghc-options:", noopCompleter)
    ]

sourceRepoFields :: Map KeyWordName Completer
sourceRepoFields = Map.fromList
    [ ("type:", constantCompleter
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
          ]),
      ("location:", noopCompleter),
      ("tag:", noopCompleter),
      ("subdir:", noopCompleter)
    ]

-- | Map, containing all stanzas in a cabal.project file as keys,
--  and lists of their possible nested keywords as values.
stanzaKeywordMap :: Map StanzaType (Map KeyWordName Completer)
stanzaKeywordMap =
  Map.fromList
    [ ("package", packageFields),
      ("program-options", packageFields),
      ("source-repository-package", sourceRepoFields)
    ]
