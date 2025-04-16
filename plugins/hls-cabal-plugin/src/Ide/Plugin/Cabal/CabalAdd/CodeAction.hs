{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Ide.Plugin.Cabal.CabalAdd.CodeAction where

import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.Aeson.Types                              (toJSON)
import           Data.Foldable                                 (asum)
import           Data.Maybe                                    (mapMaybe)
import qualified Data.Text                                     as T
import           Development.IDE.Core.PluginUtils              (uriToFilePathE)
import           Development.IDE.Types.Location                (Uri)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import qualified Distribution.Pretty                           as CabalPretty
import           Distribution.Simple.BuildTarget               (BuildTarget,
                                                                buildTargetComponentName,
                                                                readBuildTargets)
import           Distribution.Utils.Path                       (getSymbolicPath)
import           Distribution.Verbosity                        (silent,
                                                                verboseNoStderr)
import           Ide.Logger
import           Ide.Plugin.Cabal.CabalAdd.Types
import           Ide.Plugin.Cabal.Completion.Completer.Module  (fpToExposedModulePath)
import           Ide.Plugin.Cabal.Orphans                      ()
import           Ide.Plugin.Error
import           Ide.PluginUtils                               (mkLspCommand)
import           Ide.Types                                     (CommandId (CommandId),
                                                                PluginId)

import           Control.Lens                                  ((^.))
import qualified Language.LSP.Protocol.Lens                    as JL
import           Language.LSP.Protocol.Types                   (CodeActionKind (..),
                                                                VersionedTextDocumentIdentifier)
import qualified Language.LSP.Protocol.Types                   as J
import           System.FilePath
import           Text.PrettyPrint                              (render)
import           Text.Regex.TDFA

--------------------------------------------
-- Add module to cabal file
--------------------------------------------

{- | Takes a path to a cabal file, a module path in exposed module syntax
  and the contents of the cabal file and generates all possible
  code actions for inserting the module into the cabal file
  with the given contents.
-}
collectModuleInsertionOptions ::
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  PluginId ->
  VersionedTextDocumentIdentifier ->
  J.Diagnostic ->
  -- | The file path of the cabal file to insert the new module into
  FilePath ->
  -- | The generic package description of the cabal file to insert the new module into.
  GenericPackageDescription ->
  -- | The URI of the unknown haskell file/new module to insert into the cabal file.
  Uri ->
  ExceptT PluginError m [J.CodeAction]
collectModuleInsertionOptions _ plId txtDocIdentifier diag cabalFilePath gpd haskellFilePathURI = do
  haskellFilePath <- uriToFilePathE haskellFilePathURI
  let configs = concatMap (mkModuleInsertionConfig txtDocIdentifier cabalFilePath haskellFilePath) (makeStanzaItems gpd)
  pure $ map (mkCodeActionForModulePath plId diag) configs
 where
  makeStanzaItems :: GenericPackageDescription -> [StanzaItem]
  makeStanzaItems gpd =
    mainLibItem pd
      ++ libItems pd
      ++ executableItems pd
      ++ testSuiteItems pd
      ++ benchmarkItems pd
   where
    pd = flattenPackageDescription gpd

{- | Takes a buildInfo of a cabal file component as defined in the generic package description,
  and translates it to filepaths of the component's hsSourceDirs,
  to be processed for adding modules to exposed-, or other-modules fields in a cabal file.
-}
buildInfoToHsSourceDirs :: BuildInfo -> [FilePath]
buildInfoToHsSourceDirs buildInfo = map getSymbolicPath hsSourceDirs'
 where
  hsSourceDirs' = hsSourceDirs buildInfo

{- | Takes the path to the cabal file to insert the module into,
  the module path to be inserted, and a stanza representation.

  Returns a list of module insertion configs, where each config
  represents a possible place to insert the module.
-}
mkModuleInsertionConfig :: VersionedTextDocumentIdentifier -> FilePath -> FilePath -> StanzaItem -> [ModuleInsertionConfig]
mkModuleInsertionConfig txtDocIdentifier cabalFilePath haskellFilePath (StanzaItem{..}) = do
  case mkRelativeModulePathM siHsSourceDirs cabalFilePath haskellFilePath of
    Just processedModPath ->
      [modInsertItem processedModPath "other-modules"]
        ++ [modInsertItem processedModPath "exposed-modules" | CLibName _ <- [siComponent]]
    _ -> []
 where
  modInsertItem :: T.Text -> T.Text -> ModuleInsertionConfig
  modInsertItem modPath label =
    ModuleInsertionConfig
      { targetFile = cabalFilePath
      , moduleToInsert = modPath
      , modVerTxtDocId = txtDocIdentifier
      , insertionStanza = siComponent
      , insertionLabel = label
      }

mkCodeActionForModulePath :: PluginId -> J.Diagnostic -> ModuleInsertionConfig -> J.CodeAction
mkCodeActionForModulePath plId diag insertionConfig =
  J.CodeAction
    { _title = "Add to " <> label <> " as " <> fieldName
    , _kind = Just CodeActionKind_Refactor
    , _diagnostics = Just [diag]
    , _isPreferred = Nothing
    , _disabled = Nothing
    , _edit = Nothing
    , _command = Just command
    , _data_ = Nothing
    }
 where
  fieldName = insertionLabel insertionConfig
  command = mkLspCommand plId (CommandId cabalAddModuleCommandId) "Add missing module" (Just [toJSON insertionConfig])
  label = T.pack $ CabalPretty.prettyShow $ insertionStanza insertionConfig

{- | Takes a list of source subdirectories, a cabal source path and a haskell filepath
  and returns a path to the module in exposed module syntax.
  The path will be relative to one of the subdirectories, in case the module is contained within one of them.
-}
mkRelativeModulePathM :: [FilePath] -> FilePath -> FilePath -> Maybe T.Text
mkRelativeModulePathM hsSourceDirs cabalSrcPath' haskellFilePath =
  asum $
    map
      ( \srcDir -> do
          let relMP = makeRelative (normalise (cabalSrcPath </> srcDir)) haskellFilePath
          if relMP == haskellFilePath then Nothing else Just $ fpToExposedModulePath cabalSrcPath relMP
      )
      hsSourceDirs
 where
  cabalSrcPath = takeDirectory cabalSrcPath'

isUnknownModuleDiagnostic :: J.Diagnostic -> Bool
isUnknownModuleDiagnostic diag = (msg =~ regex)
 where
  msg :: T.Text
  msg = diag ^. JL.message
  regex :: T.Text
  regex = "Loading the module [\8216'][^\8217']*[\8217'] failed."

--------------------------
-- Below are several utility functions which create a StanzaItem for each of the possible Stanzas,
-- these all have specific constructors we need to match, so we can't generalise this process well.
--------------------------

benchmarkItems :: PackageDescription -> [StanzaItem]
benchmarkItems pd =
  map
    ( \benchmark ->
        StanzaItem
          { siComponent = CBenchName $ benchmarkName benchmark
          , siHsSourceDirs = buildInfoToHsSourceDirs $ benchmarkBuildInfo benchmark
          }
    )
    (benchmarks pd)

testSuiteItems :: PackageDescription -> [StanzaItem]
testSuiteItems pd =
  map
    ( \testSuite ->
        StanzaItem
          { siComponent = CTestName $ testName testSuite
          , siHsSourceDirs = buildInfoToHsSourceDirs $ testBuildInfo testSuite
          }
    )
    (testSuites pd)

executableItems :: PackageDescription -> [StanzaItem]
executableItems pd =
  map
    ( \executable ->
        StanzaItem
          { siComponent = CExeName $ exeName executable
          , siHsSourceDirs = buildInfoToHsSourceDirs $ buildInfo executable
          }
    )
    (executables pd)

libItems :: PackageDescription -> [StanzaItem]
libItems pd =
  mapMaybe
    ( \subLib ->
        case libName subLib of
          LSubLibName compName ->
            Just
              StanzaItem
                { siComponent = CLibName $ LSubLibName compName
                , siHsSourceDirs = buildInfoToHsSourceDirs $ libBuildInfo subLib
                }
          _ -> Nothing
    )
    (subLibraries pd)

mainLibItem :: PackageDescription -> [StanzaItem]
mainLibItem pd =
  case library pd of
    Just lib ->
      [ StanzaItem
          { siComponent = CLibName LMainLibName
          , siHsSourceDirs = buildInfoToHsSourceDirs $ libBuildInfo lib
          }
      ]
    Nothing -> []

--------------------------------------------
-- Add dependency to a cabal file
--------------------------------------------

{- | Creates a code action that calls the `cabalAddCommand`,
  using dependency-version suggestion pairs as input.

  Returns disabled action if no cabal files given.

  Takes haskell and cabal file paths to create a relative path
  to the haskell file, which is used to get a `BuildTarget`.
-}
addDependencySuggestCodeAction ::
  PluginId ->
  -- | Cabal's versioned text identifier
  VersionedTextDocumentIdentifier ->
  -- | A dependency-version suggestion pairs
  [(T.Text, T.Text)] ->
  -- | Path to the haskell file (source of diagnostics)
  FilePath ->
  -- | Path to the cabal file (that will be edited)
  FilePath ->
  GenericPackageDescription ->
  IO [J.CodeAction]
addDependencySuggestCodeAction plId verTxtDocId suggestions haskellFilePath cabalFilePath gpd = do
  buildTargets <- liftIO $ getBuildTargets gpd cabalFilePath haskellFilePath
  case buildTargets of
    -- If there are no build targets found, run the `cabal-add` command with default behaviour
    [] -> pure $ mkCodeActionForDependency cabalFilePath Nothing <$> suggestions
    -- Otherwise provide actions for all found targets
    targets ->
      pure $
        concat
          [ mkCodeActionForDependency cabalFilePath (Just $ buildTargetToStringRepr target)
            <$> suggestions
          | target <- targets
          ]
 where
  {- | Note the use of the `pretty` function.
   It converts the `BuildTarget` to an acceptable string representation.
   It will be used as the input for `cabal-add`'s `executeConfig`.
  -}
  buildTargetToStringRepr target = render $ CabalPretty.pretty $ buildTargetComponentName target

  {- | Finds the build targets that are used in `cabal-add`.
   Note the unorthodox usage of `readBuildTargets`:
   If the relative path to the haskell file is provided,
   `readBuildTargets` will return the build targets, this
   module is mentioned in (either exposed-modules or other-modules).
  -}
  getBuildTargets :: GenericPackageDescription -> FilePath -> FilePath -> IO [BuildTarget]
  getBuildTargets gpd cabalFilePath haskellFilePath = do
    let haskellFileRelativePath = makeRelative (dropFileName cabalFilePath) haskellFilePath
    readBuildTargets (verboseNoStderr silent) (flattenPackageDescription gpd) [haskellFileRelativePath]

  mkCodeActionForDependency :: FilePath -> Maybe String -> (T.Text, T.Text) -> J.CodeAction
  mkCodeActionForDependency cabalFilePath target (suggestedDep, suggestedVersion) =
    let
      versionTitle = if T.null suggestedVersion then T.empty else "-" <> suggestedVersion
      targetTitle = case target of
        Nothing -> T.empty
        Just t  -> " at " <> T.pack t
      title = "Add dependency " <> suggestedDep <> versionTitle <> targetTitle
      version = if T.null suggestedVersion then Nothing else Just suggestedVersion

      params =
        CabalAddDependencyCommandParams
          { depCabalPath = cabalFilePath
          , depVerTxtDocId = verTxtDocId
          , depBuildTarget = target
          , depDependency = suggestedDep
          , depVersion = version
          }
      command = mkLspCommand plId (CommandId cabalAddDependencyCommandId) "Add dependency" (Just [toJSON params])
     in
      J.CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

{- | Gives a mentioned number of @(dependency, version)@ pairs
found in the "hidden package" diagnostic message.

For example, if a ghc error looks like this:

> "Could not load module ‘Data.List.Split’
> It is a member of the hidden package ‘split-0.2.5’.
> Perhaps you need to add ‘split’ to the build-depends in your .cabal file."

or this if PackageImports extension is used:

> "Could not find module ‘Data.List.Split’
> Perhaps you meant
>   Data.List.Split (needs flag -package-id split-0.2.5)"

It extracts mentioned package names and version numbers.
In this example, it will be @[("split", "0.2.5")]@

Also supports messages without a version.

> "Perhaps you need to add ‘split’ to the build-depends in your .cabal file."

Will turn into @[("split", "")]@
-}
hiddenPackageSuggestion :: J.Diagnostic -> [(T.Text, T.Text)]
hiddenPackageSuggestion diag = getMatch (msg =~ regex)
 where
  msg :: T.Text
  msg = diag ^. JL.message
  regex :: T.Text
  regex =
    let regex' = "([a-zA-Z0-9-]*[a-zA-Z0-9])(-([0-9\\.]*))?"
     in "It is a member of the hidden package [\8216']"
          <> regex'
          <> "[\8217']"
          <> "|"
          <> "needs flag -package-id "
          <> regex'
  -- Have to do this matching because `Regex.TDFA` doesn't(?) support
  -- not-capturing groups like (?:message)
  getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [(T.Text, T.Text)]
  getMatch (_, _, _, []) = []
  getMatch (_, _, _, [dependency, _, cleanVersion, "", "", ""]) = [(dependency, cleanVersion)]
  getMatch (_, _, _, ["", "", "", dependency, _, cleanVersion]) = [(dependency, cleanVersion)]
  getMatch (_, _, _, _) = []
