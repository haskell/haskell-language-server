{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.Cabal.Definition where

import           Control.Lens                                  ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Data.List                                     (find)
import qualified Data.Maybe                                    as Maybe
import qualified Data.Text                                     as T
import           Development.IDE                               as D
import           Development.IDE.Core.PluginUtils
import qualified Distribution.Fields                           as Syntax
import           Distribution.PackageDescription               (Benchmark (..),
                                                                BuildInfo (..),
                                                                Executable (..),
                                                                ForeignLib (..),
                                                                GenericPackageDescription,
                                                                Library (..),
                                                                LibraryName (LMainLibName, LSubLibName),
                                                                PackageDescription (..),
                                                                TestSuite (..),
                                                                library,
                                                                unUnqualComponentName)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import qualified Distribution.Parsec.Position                  as Syntax
import           Distribution.Utils.Generic                    (safeHead)
import           Distribution.Utils.Path                       (getSymbolicPath)
import           Ide.Plugin.Cabal.Completion.CabalFields       as CabalFields
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalCommonSections (ParseCabalCommonSections),
                                                                ParseCabalFields (..),
                                                                ParseCabalFile (..))
import qualified Ide.Plugin.Cabal.Completion.Types             as Types
import           Ide.Plugin.Cabal.Orphans                      ()
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                    as JL
import qualified Language.LSP.Protocol.Message                 as LSP
import           Language.LSP.Protocol.Types
import           System.Directory                              (doesFileExist)
import           System.FilePath                               (joinPath,
                                                                takeDirectory,
                                                                (<.>), (</>))

-- | Handler for going to definitions.
--
-- Provides a handler for going to the definition in a cabal file,
-- gathering all possible definitions by calling subfunctions.

-- TODO: Resolve more cases for go-to definition.
gotoDefinition :: PluginMethodHandler IdeState LSP.Method_TextDocumentDefinition
gotoDefinition ide _ msgParam = do
    nfp <- getNormalizedFilePathE uri
    cabalFields <- runActionE "cabal-plugin.commonSections" ide $ useE ParseCabalFields nfp
    -- Trim the AST tree, so multiple passes in subfunctions won't hurt the performance.
    let fieldsOfInterest = maybe cabalFields (:[] ) $ CabalFields.findFieldSection cursor cabalFields

    commonSections <- runActionE "cabal-plugin.commonSections" ide $ useE ParseCabalCommonSections nfp
    let mCommonSectionsDef = gotoCommonSectionDefinition uri commonSections cursor fieldsOfInterest

    mModuleDef <- do
      mGPD <- liftIO $ runAction "cabal.GPD" ide $ useWithStale ParseCabalFile nfp
      case mGPD of
        Nothing -> pure Nothing
        Just (gpd, _) -> liftIO $ gotoModulesDefinition nfp gpd cursor fieldsOfInterest

    let defs = Maybe.catMaybes [ mCommonSectionsDef
                               , mModuleDef
                               ]
    -- Take first found definition.
    -- We assume, that there can't be multiple definitions,
    -- or the most specific definitions come first.
    case safeHead defs of
      Nothing  -> pure $ InR $ InR Null
      Just def -> pure $ InL def
    where
      cursor = Types.lspPositionToCabalPosition (msgParam ^. JL.position)
      uri = msgParam ^. JL.textDocument . JL.uri

-- | Definitions for Sections.
--
-- Provides a Definition if cursor is pointed at an identifier,
-- otherwise gives Nothing.
-- The definition is found by traversing the sections and comparing their name to
-- the clicked identifier.
gotoCommonSectionDefinition
  :: Uri -- ^ Cabal file URI
  -> [Syntax.Field Syntax.Position] -- ^ Found common sections
  -> Syntax.Position -- ^ Cursor position
  -> [Syntax.Field Syntax.Position] -- ^ Trimmed cabal AST on a cursor
  -> Maybe Definition
gotoCommonSectionDefinition uri commonSections cursor fieldsOfInterest = do
  cursorText <- CabalFields.findTextWord cursor fieldsOfInterest
  commonSection <- find (isSectionArgName cursorText) commonSections
  Just $ Definition $ InL $ Location uri $ CabalFields.getFieldLSPRange commonSection
  where
    isSectionArgName name (Syntax.Section _ sectionArgName _) = name == CabalFields.onelineSectionArgs sectionArgName
    isSectionArgName _ _ = False

-- | Definitions for Modules.
--
-- Provides a Definition if cursor is pointed at a
-- exposed-module or other-module field, otherwise gives Nothing
--
-- Definition is found by looking for a module name,
-- the cursor is pointing to and looking for it in @BuildInfo@s.
-- Note that since a trimmed ast is provided, a @Definition@ to
-- a module with the same name as the target one,
-- but in another build target can't be given.
--
-- See resolving @Config@ module in tests.
gotoModulesDefinition
  :: NormalizedFilePath -- ^ Normalized FilePath to the cabal file
  -> GenericPackageDescription
  -> Syntax.Position -- ^ Cursor position
  -> [Syntax.Field Syntax.Position] -- ^ Trimmed cabal AST on a cursor
  -> IO (Maybe Definition)
gotoModulesDefinition nfp gpd cursor fieldsOfInterest = do
  let mCursorText = CabalFields.findTextWord cursor fieldsOfInterest
      moduleNames = CabalFields.getModulesNames fieldsOfInterest
      mModuleName = find (isModuleName mCursorText) moduleNames

  case mModuleName of
    Nothing -> pure Nothing
    Just (mBuildTargetNames, moduleName) -> do
      let buildInfos = foldMap (lookupBuildTargetPackageDescription
                                      (flattenPackageDescription gpd))
                                      mBuildTargetNames
          sourceDirs = map getSymbolicPath $ concatMap hsSourceDirs buildInfos
          potentialPaths = map (\dir -> takeDirectory (fromNormalizedFilePath nfp) </> dir </> toHaskellFile moduleName) sourceDirs
      allPaths <- liftIO $ filterM doesFileExist potentialPaths
      -- Don't provide the range, since there is little benefit for it
      let locations = map (\pth -> Location (filePathToUri pth) (mkRange 0 0 0 0)) allPaths
      case safeHead locations of -- We assume there could be only one source location
        Nothing       -> pure Nothing
        Just location -> pure $ Just $ Definition $ InL location
  where
    isModuleName (Just name) (_,  moduleName) = name == moduleName
    isModuleName _ _                          = False

-- | Gives all `buildInfo`s given a target name.
--
-- `Maybe buildTargetName` is provided, and if it's
-- Nothing we assume, that it's a main library.
-- Otherwise looks for the provided name.
lookupBuildTargetPackageDescription :: PackageDescription -> Maybe T.Text -> [BuildInfo]
lookupBuildTargetPackageDescription (PackageDescription {..}) Nothing =
  case library of
    Nothing                       -> [] -- Target is a main library but no main library was found
    Just (Library {libBuildInfo}) -> [libBuildInfo]
lookupBuildTargetPackageDescription (PackageDescription {..}) (Just buildTargetName) =
  Maybe.catMaybes $
    map executableNameLookup executables <>
    map subLibraryNameLookup subLibraries <>
    map foreignLibsNameLookup foreignLibs <>
    map testSuiteNameLookup testSuites <>
    map benchmarkNameLookup benchmarks
  where
    executableNameLookup :: Executable -> Maybe BuildInfo
    executableNameLookup (Executable {exeName, buildInfo}) =
      if T.pack (unUnqualComponentName exeName) == buildTargetName
        then Just buildInfo
        else Nothing
    subLibraryNameLookup :: Library -> Maybe BuildInfo
    subLibraryNameLookup (Library {libName, libBuildInfo}) =
      case libName of
        (LSubLibName name) ->
          if T.pack (unUnqualComponentName name) == buildTargetName
            then Just libBuildInfo
            else Nothing
        LMainLibName -> Nothing
    foreignLibsNameLookup :: ForeignLib -> Maybe BuildInfo
    foreignLibsNameLookup (ForeignLib {foreignLibName, foreignLibBuildInfo}) =
        if T.pack (unUnqualComponentName foreignLibName) == buildTargetName
        then Just foreignLibBuildInfo
        else Nothing
    testSuiteNameLookup :: TestSuite -> Maybe BuildInfo
    testSuiteNameLookup (TestSuite {testName, testBuildInfo}) =
      if T.pack (unUnqualComponentName testName) == buildTargetName
        then Just testBuildInfo
        else Nothing
    benchmarkNameLookup :: Benchmark -> Maybe BuildInfo
    benchmarkNameLookup (Benchmark {benchmarkName, benchmarkBuildInfo}) =
        if T.pack (unUnqualComponentName benchmarkName) == buildTargetName
        then Just benchmarkBuildInfo
        else Nothing

-- | Converts a name of a module to a FilePath.
-- Is needed to guess the relative path to a file
-- using the name of the module.
-- We assume, that correct module naming is guaranteed.
--
-- Warning: Generally not advised to use, if there are
-- better ways to get the path.
--
-- Examples: (output is system dependent)
--
-- >>> toHaskellFile "My.Module.Lib"
-- "My/Module/Lib.hs"
-- >>> toHaskellFile "Main"
-- "Main.hs"
toHaskellFile :: T.Text -> FilePath
toHaskellFile moduleName = joinPath (map T.unpack $ T.splitOn "." moduleName) <.> ".hs"
