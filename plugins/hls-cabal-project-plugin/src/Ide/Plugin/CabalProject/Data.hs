{-# LANGUAGE OverloadedStrings #-}

-- | Completion (and validation) tables for @cabal.project@ files.
--
-- The keys of these maps are the single source of truth for which fields and
-- sections are considered known by the diagnostics of this plugin.
module Ide.Plugin.CabalProject.Data
  ( projectTopLevelFields
  , projectStanzaKeywordMap
  , projectSectionNames
  , boolCompleter
  ) where

import           Data.Map                                       (Map)
import qualified Data.Map                                       as Map
import           Data.Text                                      (Text)
import qualified Data.Text                                      as T
import qualified Distribution.Types.SourceRepo                  as SourceRepo
import           Ide.Plugin.Cabal.Completion.Completer.FilePath (filePathCompleter)
import           Ide.Plugin.Cabal.Completion.Completer.Simple   (constantCompleter,
                                                                 noopCompleter)
import           Ide.Plugin.Cabal.Completion.Completer.Types    (Completer)
import           Ide.Plugin.Cabal.Completion.Types              (KeyWordName,
                                                                 StanzaType)

-- | Completer for boolean-valued fields.
boolCompleter :: Completer
boolCompleter = constantCompleter ["True", "False"]

-- | Fields valid at the top level of a @cabal.project@ file.
-- Fields from 'packageConfigFields' are also valid at the top level and are
-- therefore included here.
projectTopLevelFields :: Map KeyWordName Completer
projectTopLevelFields = projectOnlyFields <> packageConfigFields

-- | Fields only valid at the top level of a @cabal.project@ file.
projectOnlyFields :: Map KeyWordName Completer
projectOnlyFields = Map.fromList $
  -- path-valued fields
  [ ("packages:", filePathCompleter)
  , ("optional-packages:", filePathCompleter)
  , ("import:", filePathCompleter)
  , ("store-dir:", filePathCompleter)
  , ("symlink-bindir:", filePathCompleter)
  , ("installdir:", filePathCompleter)
  , ("package-env:", filePathCompleter)
  -- fields without structured values
  , ("extra-packages:", noopCompleter)
  , ("constraints:", noopCompleter)
  , ("preferences:", noopCompleter)
  , ("allow-newer:", noopCompleter)
  , ("allow-older:", noopCompleter)
  , ("active-repositories:", noopCompleter)
  , ("index-state:", noopCompleter)
  , ("package-dbs:", noopCompleter)
  , ("verbose:", noopCompleter)
  , ("jobs:", noopCompleter)
  , ("max-backjumps:", noopCompleter)
  , ("cabal-lib-version:", noopCompleter)
  , ("with-compiler:", noopCompleter)
  , ("with-hc-pkg:", noopCompleter)
  , ("remote-repo-cache:", noopCompleter)
  , ("logs-dir:", noopCompleter)
  , ("build-summary:", noopCompleter)
  , ("build-log:", noopCompleter)
  , ("builddir:", noopCompleter)
  , ("root-cmd:", noopCompleter)
  , ("test-options:", noopCompleter)
  , ("benchmark-options:", noopCompleter)
  -- boolean fields
  , ("keep-going:", boolCompleter)
  , ("semaphore:", boolCompleter)
  , ("build-timings:", boolCompleter)
  , ("lib:", boolCompleter)
  , ("offline:", boolCompleter)
  , ("ignore-expiry:", boolCompleter)
  , ("prefer-oldest:", boolCompleter)
  , ("strong-flags:", boolCompleter)
  , ("reorder-goals:", boolCompleter)
  , ("count-conflicts:", boolCompleter)
  , ("fine-grained-conflicts:", boolCompleter)
  , ("minimize-conflict-set:", boolCompleter)
  , ("allow-boot-library-installs:", boolCompleter)
  , ("open:", boolCompleter)
  , ("report-planning-failure:", boolCompleter)
  -- enumeration fields
  , ("reject-unconstrained-dependencies:", constantCompleter ["all", "none"])
  , ("write-ghc-environment-files:", constantCompleter ["never", "always", "ghc8.4.4+"])
  , ("http-transport:", constantCompleter ["curl", "wget", "powershell", "plain-http"])
  , ("solver:", constantCompleter ["modular"])
  , ("remote-build-reporting:", constantCompleter ["none", "basic", "detailed", "anonymous"])
  , ("test-show-details:", constantCompleter ["never", "immediate", "always", "failures"])
  , ("install-method:", constantCompleter ["copy", "symlink"])
  , ("overwrite-policy:", constantCompleter ["always", "never", "prompt"])
  , ("prefer-version:", constantCompleter ["oldest", "latest", "installed-or-latest"])
  ]

-- | Fields valid inside a @package@ stanza of a @cabal.project@ file.
-- These are the same fields as in a project-level context, so they are also
-- valid at the top level.
packageConfigFields :: Map KeyWordName Completer
packageConfigFields = Map.fromList $
  -- boolean fields
  [ ("tests:", boolCompleter)
  , ("benchmarks:", boolCompleter)
  , ("coverage:", boolCompleter)
  , ("run-tests:", boolCompleter)
  , ("documentation:", boolCompleter)
  , ("library-coverage:", boolCompleter)
  , ("profiling:", boolCompleter)
  , ("library-profiling:", boolCompleter)
  , ("executable-profiling:", boolCompleter)
  , ("library-vanilla:", boolCompleter)
  , ("shared:", boolCompleter)
  , ("static:", boolCompleter)
  , ("executable-dynamic:", boolCompleter)
  , ("executable-static:", boolCompleter)
  , ("executable-stripping:", boolCompleter)
  , ("library-stripping:", boolCompleter)
  , ("library-for-ghci:", boolCompleter)
  , ("split-sections:", boolCompleter)
  , ("split-objs:", boolCompleter)
  , ("build-info:", boolCompleter)
  , ("haddock-hoogle:", boolCompleter)
  , ("haddock-html:", boolCompleter)
  , ("haddock-quickjump:", boolCompleter)
  , ("haddock-executables:", boolCompleter)
  , ("haddock-tests:", boolCompleter)
  , ("haddock-benchmarks:", boolCompleter)
  , ("haddock-internal:", boolCompleter)
  , ("haddock-all:", boolCompleter)
  , ("haddock-hyperlink-source:", boolCompleter)
  , ("haddock-keep-temp-files:", boolCompleter)
  , ("library-bytecode:", boolCompleter)
  , ("relocatable:", boolCompleter)
  -- enumeration fields
  , ("optimization:", constantCompleter ["0", "1", "2"])
  , ("debug-info:", constantCompleter ["0", "1", "2", "3"])
  , ("profiling-detail:", constantCompleter ["none", "default", "functions", "cafes", "all"])
  , ("library-profiling-detail:", constantCompleter ["none", "default", "functions", "cafes", "all"])
  , ("compiler:", constantCompleter ["ghc", "ghcjs"])
  -- free-form fields
  , ("configure-options:", noopCompleter)
  , ("ghc-options:", noopCompleter)
  , ("program-ghc-options:", noopCompleter)
  , ("flags:", noopCompleter)
  , ("program-prefix:", noopCompleter)
  , ("program-suffix:", noopCompleter)
  , ("doc-index-file:", noopCompleter)
  , ("haddock-css:", noopCompleter)
  , ("haddock-html-location:", noopCompleter)
  , ("haddock-contents-location:", noopCompleter)
  , ("haddock-hscolour-css:", noopCompleter)
  , ("haddock-use-unicode:", boolCompleter)
  -- path fields
  , ("extra-prog-path:", filePathCompleter)
  , ("haddock-output-dir:", filePathCompleter)
  , ("haddock-resources-dir:", filePathCompleter)
  , ("extra-include-dirs:", filePathCompleter)
  , ("extra-lib-dirs:", filePathCompleter)
  , ("extra-framework-dirs:", filePathCompleter)
  ]

-- | Fields valid inside the stanzas (sections) of a @cabal.project@ file.
projectStanzaKeywordMap :: Map StanzaType (Map KeyWordName Completer)
projectStanzaKeywordMap = Map.fromList
  [ ( "package"
    , packageConfigFields
    )
  , ( "source-repository-package"
    , Map.fromList
        [ ("type:", constantCompleter (map (T.pack . show) SourceRepo.knownRepoTypes))
        , ("location:", noopCompleter)
        , ("tag:", noopCompleter)
        , ("post-checkout-command:", noopCompleter)
        , ("branch:", noopCompleter)
        , ("subdir:", filePathCompleter)
        ]
    )
  , ( "repository"
    , Map.fromList
        [ ("url:", noopCompleter)
        , ("secure:", boolCompleter)
        , ("root-keys:", noopCompleter)
        , ("key-threshold:", noopCompleter)
        ]
    )
  , ( "program-options"
    , programOptionsFields
    )
  ]

-- | Sub-fields of a @program-options@ stanza: options for external programs
-- cabal invokes (shared with the @program-options@ stanza of the cabal config
-- file).
programOptionsFields :: Map KeyWordName Completer
programOptionsFields = Map.fromList
  [ (name <> ":", noopCompleter)
  | name <-
      [ "ghc-options"
      , "ghc-prof-options"
      , "ghc-shared-options"
      , "ghcjs-options"
      , "ghcjs-prof-options"
      , "ld-options"
      , "cpp-options"
      , "cc-options"
      , "haddock-options"
      , "happy-options"
      , "alex-options"
      , "strip-options"
      ]
  ]

-- | Sections that may appear in a @cabal.project@ file.
projectSectionNames :: [Text]
projectSectionNames = ["package", "source-repository-package", "repository", "program-options"]
