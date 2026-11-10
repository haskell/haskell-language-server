{-# LANGUAGE OverloadedStrings #-}

-- | Hover documentation for @cabal.project@ fields, sections and keywords.
--
-- Summaries are transcribed from the @:synopsis:@ fields of the Cabal cabal.project reference,
-- doc/cabal-project-description-file.rst on master; links target the master-built rendering:
-- https://cabal.readthedocs.io/en/latest/cabal-project-description-file.html
module Ide.Plugin.CabalProject.Docs
  ( ProjectFieldDoc (..)
  , projectFieldDocs
  , projectSectionDocs
  , conditionalDoc
  , projectHover
  , renderHover
  , conditionalKeywords
  , fallbackSummary
  , docsIndexUrl
  , docUrl
  ) where

import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Data.Text.Encoding                      (decodeUtf8)
import qualified Distribution.Fields                     as Syntax
import qualified Distribution.Parsec.Position            as Syntax
import qualified Ide.Plugin.Cabal.Completion.CabalFields as CabalFields
import           Ide.Plugin.Cabal.Outline                (addNameLengthToLSPRange,
                                                          cabalPositionToLSPRange)
import           Language.LSP.Protocol.Types

data ProjectFieldDoc = ProjectFieldDoc
  { docSummary :: Text
  , docAnchor  :: Maybe Text
  }

-- | Base URL of the Cabal cabal.project reference page; 'fallbackSummary'
-- marks entries not documented there (page link only).
docsIndexUrl :: Text
docsIndexUrl = "https://cabal.readthedocs.io/en/latest/cabal-project-description-file.html"

-- | Just anchor -> deep link into cabal-project-description-file.html; Nothing -> page itself.
docUrl :: Maybe Text -> Text
docUrl (Just anchor) = docsIndexUrl <> "#" <> anchor
docUrl Nothing       = docsIndexUrl

fallbackSummary :: Text
fallbackSummary = "(not documented in the Cabal user guide)"

doc :: Text -> Text -> ProjectFieldDoc
doc summary anchor = ProjectFieldDoc summary (Just anchor)

fallback :: Maybe Text -> ProjectFieldDoc
fallback anchor = ProjectFieldDoc fallbackSummary anchor

-- | Docs for every field of 'Data.projectTopLevelFields' and every stanza map
-- in 'Data.projectStanzaKeywordMap'. Keys include the trailing colon.
-- The reverse-coverage test in @test/Main.hs@ asserts that every key here is
-- accepted by the diagnostics' known-field tables
-- ('Data.projectTopLevelFields' or a stanza map in
-- 'Data.projectStanzaKeywordMap').
projectFieldDocs :: Map Text ProjectFieldDoc
projectFieldDocs = Map.fromList
  -- project-only fields
  [ ("packages:", doc "Project packages." "cfg-field-packages")
  , ("optional-packages:", doc "Optional project packages." "cfg-field-optional-packages")
  , ("import:", doc "Import another cabal.project or freeze file (local path or URL)." "conditionals-and-imports")
  , ("extra-packages:", doc "Adds external packages as local" "cfg-field-extra-packages")
  , ("constraints:", doc "Extra dependencies constraints." "cfg-field-constraints")
  , ("preferences:", doc "Preferred dependency versions." "cfg-field-preferences")
  , ("allow-newer:", doc "Lift dependencies upper bound constraints." "cfg-field-allow-newer")
  , ("allow-older:", doc "Lift dependency lower bound constraints." "cfg-field-allow-older")
  , ("active-repositories:", doc "Specify active package repositories" "cfg-field-active-repositories")
  , ("index-state:", doc "Use source package index state as it existed at a previous time." "cfg-field-index-state")
  , ("package-dbs:", doc "PackageDB stack manipulation" "cfg-field-package-dbs")
  , ("verbose:", doc "Build verbosity level." "cfg-field-verbose")
  , ("jobs:", doc "Number of builds running in parallel." "cfg-field-jobs")
  , ("semaphore:", doc "Use GHC's support for semaphore based parallelism." "cfg-field-semaphore")
  , ("build-timings:", doc "Log timing information to stdout." "cfg-field-build-timings")
  , ("max-backjumps:", doc "Maximum number of solver backjumps." "cfg-field-max-backjumps")
  , ("cabal-lib-version:", doc "Version of Cabal library used to build package." "cfg-field-cabal-lib-version")
  , ("with-compiler:", doc "Path to compiler executable." "cfg-field-with-compiler")
  , ("with-hc-pkg:", doc "Path to package tool." "cfg-field-with-hc-pkg")
  , ("remote-repo-cache:", doc "Location of packages cache." "cfg-field-remote-repo-cache")
  , ("logs-dir:", doc "Directory to store build logs." "cfg-field-logs-dir")
  , ("build-summary:", doc "Build summaries location." "cfg-field-build-summary")
  , ("build-log:", doc "Build log location template." "cfg-field-build-log")
  , ("builddir:", doc "Specifies the name of the directory where build products for build will be stored; defaults to @dist-newstyle@." "cfg-field-builddir")
  , ("store-dir:", doc "Specifies the name of the directory of the global package store." "cfg-field-store-dir")
  , ("package-env:", doc "Package environment file to create or modify." "cfg-field-package-env")
  , ("root-cmd:", fallback Nothing)
  , ("symlink-bindir:", doc "Add symlinks to installed executables into this directory." "cfg-field-symlink-bindir")
  , ("installdir:", doc "Target directory for installed executables." "cfg-field-installdir")
  , ("install-method:", doc "How to install executables." "cfg-field-install-method")
  , ("overwrite-policy:", doc "How to handle existing executable links." "cfg-field-overwrite-policy")
  , ("lib:", doc "Install libraries instead of executables." "cfg-field-lib")
  , ("test-options:", fallback Nothing)
  , ("benchmark-options:", fallback Nothing)
  , ("keep-going:", doc "Try to continue building on failure." "cfg-field-keep-going")
  , ("offline:", doc "Disable package downloads from the network." "cfg-field-offline")
  , ("ignore-expiry:", doc "Ignore Hackage expiration dates." "cfg-field-ignore-expiry")
  , ("prefer-oldest:", doc "Prefer the oldest versions of packages available." "cfg-field-prefer-oldest")
  , ("prefer-version:", doc "Specify how to pick package versions" "cfg-field-prefer-version")
  , ("strong-flags:", doc "Do not defer flag choices when solving." "cfg-field-strong-flags")
  , ("reorder-goals:", doc "Allow solver to reorder goals." "cfg-field-reorder-goals")
  , ("count-conflicts:", doc "Solver prefers versions with less conflicts." "cfg-field-count-conflicts")
  , ("fine-grained-conflicts:", doc "Skip a version of a package if it does not resolve any conflicts encountered in the last version (solver optimization)." "cfg-field-fine-grained-conflicts")
  , ("minimize-conflict-set:", doc "Try to improve the solver error message when there is no solution." "cfg-field-minimize-conflict-set")
  , ("allow-boot-library-installs:", doc "Allow cabal to install or upgrade any package." "cfg-field-allow-boot-library-installs")
  , ("open:", doc "Open generated documentation in-browser." "cfg-field-open")
  , ("report-planning-failure:", doc "Report dependency solving failures." "cfg-field-report-planning-failure")
  , ("reject-unconstrained-dependencies:", doc "Restrict the solver to packages that have constraints on them." "cfg-field-reject-unconstrained-dependencies")
  , ("write-ghc-environment-files:", doc "Whether a @.ghc.environment@ should be created after a successful build." "cfg-field-write-ghc-environment-files")
  , ("http-transport:", doc "Transport to use with http(s) requests." "cfg-field-http-transport")
  , ("solver:", doc "Which solver to use." "cfg-field-solver")
  , ("remote-build-reporting:", doc "Build report level for remote reporting." "cfg-field-remote-build-reporting")
  , ("test-show-details:", fallback Nothing)
  -- package stanza fields
  , ("tests:", doc "Build tests." "cfg-field-tests")
  , ("benchmarks:", doc "Build benchmarks." "cfg-field-benchmarks")
  , ("coverage:", doc "Build with coverage enabled." "cfg-field-coverage")
  , ("run-tests:", doc "Run package test suite during installation." "cfg-field-run-tests")
  , ("documentation:", doc "Enable building of documentation." "cfg-field-documentation")
  , ("library-coverage:", doc "Deprecated, use coverage." "cfg-field-library-coverage")
  , ("profiling:", doc "Enable profiling builds." "cfg-field-profiling")
  , ("library-profiling:", doc "Build libraries with profiling enabled." "cfg-field-library-profiling")
  , ("executable-profiling:", doc "Build executables with profiling enabled." "cfg-field-executable-profiling")
  , ("library-vanilla:", doc "Build libraries without profiling." "cfg-field-library-vanilla")
  , ("shared:", doc "Build shared library." "cfg-field-shared")
  , ("static:", doc "Build static library." "cfg-field-static")
  , ("executable-dynamic:", doc "Link executables dynamically." "cfg-field-executable-dynamic")
  , ("executable-static:", doc "Build fully static executables." "cfg-field-executable-static")
  , ("executable-stripping:", doc "Strip installed programs." "cfg-field-executable-stripping")
  , ("library-stripping:", doc "Strip installed libraries." "cfg-field-library-stripping")
  , ("library-for-ghci:", doc "Build libraries suitable for use with GHCi." "cfg-field-library-for-ghci")
  , ("split-sections:", doc "Use GHC's split sections feature." "cfg-field-split-sections")
  , ("split-objs:", doc "Use GHC's split objects feature." "cfg-field-split-objs")
  , ("library-bytecode:", doc "Build bytecode libraries." "cfg-field-library-bytecode")
  , ("relocatable:", doc "Build relocatable package." "cfg-field-relocatable")
  , ("build-info:", doc "Whether build information for each individual component should be written in a machine readable format." "cfg-field-build-info")
  , ("haddock-hoogle:", doc "Generate Hoogle file." "cfg-field-haddock-hoogle")
  , ("haddock-html:", doc "Build HTML documentation." "cfg-field-haddock-html")
  , ("haddock-quickjump:", doc "Generate Quickjump file." "cfg-field-haddock-quickjump")
  , ("haddock-executables:", doc "Generate documentation for executables." "cfg-field-haddock-executables")
  , ("haddock-tests:", doc "Generate documentation for tests." "cfg-field-haddock-tests")
  , ("haddock-benchmarks:", doc "Generate documentation for benchmarks." "cfg-field-haddock-benchmarks")
  , ("haddock-internal:", doc "Generate documentation for internal modules" "cfg-field-haddock-internal")
  , ("haddock-all:", doc "Generate documentation for everything" "cfg-field-haddock-all")
  , ("haddock-hyperlink-source:", doc "Generate hyperlinked source code for documentation" "cfg-field-haddock-hyperlink-source")
  , ("haddock-keep-temp-files:", doc "Keep temporary Haddock files." "cfg-field-haddock-keep-temp-files")
  , ("optimization:", doc "Build with optimization." "cfg-field-optimization")
  , ("debug-info:", doc "Build with debug info enabled." "cfg-field-debug-info")
  , ("profiling-detail:", doc "Profiling detail level." "cfg-field-profiling-detail")
  , ("library-profiling-detail:", doc "Libraries profiling detail level." "cfg-field-library-profiling-detail")
  , ("compiler:", doc "Compiler to build with." "cfg-field-compiler")
  , ("configure-options:", doc "Options to pass to configure script." "cfg-field-configure-options")
  , ("ghc-options:", fallback Nothing)
  , ("ghc-prof-options:", fallback Nothing)
  , ("ghc-shared-options:", fallback Nothing)
  , ("ghcjs-options:", fallback Nothing)
  , ("ghcjs-prof-options:", fallback Nothing)
  , ("ld-options:", fallback Nothing)
  , ("cpp-options:", fallback Nothing)
  , ("cc-options:", fallback Nothing)
  , ("haddock-options:", fallback Nothing)
  , ("happy-options:", fallback Nothing)
  , ("alex-options:", fallback Nothing)
  , ("strip-options:", fallback Nothing)
  , ("program-ghc-options:", fallback Nothing)
  , ("flags:", doc "Enable or disable package flags." "cfg-field-flags")
  , ("program-prefix:", doc "Prepend prefix to program names." "cfg-field-program-prefix")
  , ("program-suffix:", doc "Append suffix to program names." "cfg-field-program-suffix")
  , ("doc-index-file:", doc "Path to haddock templates." "cfg-field-doc-index-file")
  , ("haddock-css:", doc "Location of Haddock CSS file." "cfg-field-haddock-css")
  , ("haddock-html-location:", doc "Location of HTML documentation for prerequisite packages." "cfg-field-haddock-html-location")
  , ("haddock-output-dir:", doc "Generate haddock documentation into this directory." "cfg-field-haddock-output-dir")
  , ("haddock-use-unicode:", doc "Pass --use-unicode option to haddock." "cfg-field-haddock-use-unicode")
  , ("haddock-resources-dir:", doc "Location of Haddock's static/auxiliary files." "cfg-field-haddock-resources-dir")
  , ("haddock-contents-location:", doc "URL for contents page." "cfg-field-haddock-contents-location")
  , ("haddock-hscolour-css:", doc "Location of CSS file for HsColour" "cfg-field-haddock-hscolour-css")
  , ("extra-prog-path:", doc "Add directories to program search path." "cfg-field-extra-prog-path")
  , ("extra-include-dirs:", doc "Adds C header search path." "cfg-field-extra-include-dirs")
  , ("extra-lib-dirs:", doc "Adds library search directory." "cfg-field-extra-lib-dirs")
  , ("extra-framework-dirs:", doc "Adds framework search directory (OS X only)." "cfg-field-extra-framework-dirs")
  -- source-repository-package stanza fields
  , ("type:", fallback (Just "cfg-field-type"))
  , ("location:", fallback (Just "cfg-field-location"))
  , ("tag:", fallback (Just "cfg-field-tag"))
  , ("subdir:", fallback (Just "cfg-field-subdir"))
  , ("post-checkout-command:", doc "Run command in the checked out repository, prior to sdisting." "cfg-field-post-checkout-command")
  , ("branch:", fallback (Just "cfg-field-branch"))
  -- repository stanza fields
  , ("url:", fallback Nothing)
  , ("secure:", fallback Nothing)
  , ("root-keys:", fallback Nothing)
  , ("key-threshold:", fallback Nothing)
  ]

-- | Docs for section names.
projectSectionDocs :: Map Text ProjectFieldDoc
projectSectionDocs = Map.fromList
  [ ("package", doc "Package configuration options." "cfg-section-package-package")
  , ("source-repository-package", doc "Build a package from a remote version control location." "pkg-consume-source")
  , ("program-options", doc "Provide options for external programs for all local packages." "program-options")
  , ("repository", fallback Nothing)
  ]

-- | Keywords introducing conditional stanzas (@if@/@elif@/@else@),
-- shared with completion-context classification.
conditionalKeywords :: [Text]
conditionalKeywords = ["if", "elif", "else"]

-- | Doc for conditional keywords @if@/@elif@/@else@.
conditionalDoc :: ProjectFieldDoc
conditionalDoc = ProjectFieldDoc "Conditionally apply the enclosed fields when the predicate holds." Nothing

-- | Documentation for the field/section name under the cursor.
-- Returns the rendered markdown and the LSP range covering the name.
projectHover :: Syntax.Position -> [Syntax.Field Syntax.Position] -> Maybe (Text, Range)
projectHover pos = go . CabalFields.findFieldSection pos
  where
    onName namePos name =
      Syntax.positionRow namePos == Syntax.positionRow pos
        && Syntax.positionCol namePos <= Syntax.positionCol pos
        && Syntax.positionCol pos <= Syntax.positionCol namePos + T.length (decodeUtf8 name)
    nameRange namePos n = cabalPositionToLSPRange namePos `addNameLengthToLSPRange` n
    -- findFieldSection returns the enclosing section for any cursor line
    -- inside it; hover on the section name, otherwise descend into its fields.
    go (Just (Syntax.Section (Syntax.Name secPos name) _ inner))
      | not (onName secPos name) = go (CabalFields.findFieldSection pos inner)
      | let n = decodeUtf8 name
      , Just d <- Map.lookup n projectSectionDocs =
          Just (renderNameDoc n "(section)" d, nameRange secPos n)
      | let n = decodeUtf8 name
      , n `elem` conditionalKeywords = Just (renderNameDoc n "" conditionalDoc, nameRange secPos n)
      | otherwise = Nothing
    go (Just (Syntax.Field (Syntax.Name fPos name) _))
      | onName fPos name
      , let n = decodeUtf8 name
      , Just d <- Map.lookup (n <> ":") projectFieldDocs =
          Just (renderNameDoc n "" d, nameRange fPos n)
      | otherwise = Nothing
    go Nothing = Nothing

-- | Render hover markup: backticked name with optional tag, italic summary,
-- and a deep link into the Cabal cabal.project reference.
renderNameDoc :: Text -> Text -> ProjectFieldDoc -> Text
renderNameDoc name tag d =
  "`" <> name <> "`" <> tag
    <> " — _" <> docSummary d <> "_\n\n"
    <> "[cabal.project reference](" <> docUrl (docAnchor d) <> ")"

renderHover :: Text -> Range -> Hover
renderHover markup rng = Hover (InL (MarkupContent MarkupKind_Markdown markup)) (Just rng)
