{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.Cabal.Definition where

import           Control.Concurrent.Strict
import           Control.DeepSeq
import           Control.Lens                                  ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe                     (runMaybeT)
import qualified Data.ByteString                               as BS
import           Data.Hashable
import           Data.HashMap.Strict                           (HashMap)
import qualified Data.HashMap.Strict                           as HashMap
import           Data.List                                     (find)
import qualified Data.List.NonEmpty                            as NE
import qualified Data.Maybe                                    as Maybe
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as Encoding
import           Data.Typeable
import           Development.IDE                               as D
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.Shake                    (restartShakeSession)
import qualified Development.IDE.Core.Shake                    as Shake
import           Development.IDE.Graph                         (Key,
                                                                alwaysRerun)
import qualified Development.IDE.Plugin.Completions.Logic      as Ghcide
import           Development.IDE.Types.Shake                   (toKey)
import qualified Distribution.Fields                           as Syntax
import           Distribution.PackageDescription               (Benchmark (..),
                                                                BuildInfo (..),
                                                                Executable (..),
                                                                ForeignLib (..),
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
import           GHC.Generics
import           Ide.Plugin.Cabal.Completion.CabalFields       as CabalFields
import qualified Ide.Plugin.Cabal.Completion.Completer.Types   as CompleterTypes
import qualified Ide.Plugin.Cabal.Completion.Completions       as Completions
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalCommonSections (ParseCabalCommonSections),
                                                                ParseCabalFields (..),
                                                                ParseCabalFile (..))
import qualified Ide.Plugin.Cabal.Completion.Types             as Types
import qualified Ide.Plugin.Cabal.Diagnostics                  as Diagnostics
import qualified Ide.Plugin.Cabal.FieldSuggest                 as FieldSuggest
import qualified Ide.Plugin.Cabal.LicenseSuggest               as LicenseSuggest
import           Ide.Plugin.Cabal.Orphans                      ()
import           Ide.Plugin.Cabal.Outline
import qualified Ide.Plugin.Cabal.Parse                        as Parse
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                    as JL
import qualified Language.LSP.Protocol.Message                 as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.VFS                              as VFS
import           System.Directory                              (doesFileExist)
import           System.FilePath                               (takeDirectory,
                                                                (</>), (<.>), joinPath)

-- | CodeActions for going to definitions.
--
-- Provides a CodeAction for going to a definition when clicking on an identifier
-- and clicking on exposed-module or other-module field.
-- The definition is found by traversing the sections and comparing their name to
-- the clicked identifier. If it's not in sections it attempts to find it in module names.
--
-- TODO: Resolve more cases for go-to definition.
gotoDefinition :: PluginMethodHandler IdeState LSP.Method_TextDocumentDefinition
gotoDefinition ideState _ msgParam = do
    nfp <- getNormalizedFilePathE uri
    cabalFields <- runActionE "cabal-plugin.commonSections" ideState $ useE ParseCabalFields nfp
    case CabalFields.findTextWord cursor cabalFields of
      Nothing ->
        pure $ InR $ InR Null
      Just cursorText -> do
        commonSections <- runActionE "cabal-plugin.commonSections" ideState $ useE ParseCabalCommonSections nfp
        case find (isSectionArgName cursorText) commonSections of
          Just commonSection -> do
            pure $ InL $ Definition $ InL $ Location uri $ CabalFields.getFieldLSPRange commonSection

          Nothing -> do
            let moduleNames = CabalFields.getModulesNames cabalFields
                mModuleName = find (isModuleName cursorText) moduleNames
            case mModuleName of
              Nothing -> pure $ InR $ InR Null
              Just (mBuildTargetNames, moduleName) -> do
                mGPD <- liftIO $ runAction "cabal.GPD" ideState $ useWithStale ParseCabalFile nfp
                case mGPD of
                  Nothing -> pure $ InR $ InR Null
                  Just (gpd, _) -> do
                    let buildInfos = foldMap (lookupBuildTargetPackageDescription
                                                    (flattenPackageDescription gpd))
                                                    mBuildTargetNames
                        sourceDirs = map getSymbolicPath $ concatMap hsSourceDirs buildInfos
                        potentialPaths = map (\dir -> takeDirectory (fromNormalizedFilePath nfp) </> dir </> toHaskellFile moduleName) sourceDirs
                    allPaths <- liftIO $ filterM doesFileExist potentialPaths
                    let locations = map (\pth -> Location (filePathToUri pth) (mkRange 0 0 0 0)) allPaths
                    case safeHead locations of -- We assume there could be only one source location
                      Nothing       -> pure $ InR $ InR Null
                      Just location -> pure $ InL $ Definition $ InL location
    where
      cursor = Types.lspPositionToCabalPosition (msgParam ^. JL.position)
      uri = msgParam ^. JL.textDocument . JL.uri
      isSectionArgName name (Syntax.Section _ sectionArgName _) = name == CabalFields.onelineSectionArgs sectionArgName
      isSectionArgName _ _ = False
      isModuleName name (_,  moduleName) = name == moduleName

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

-- | Converts a name of a module to a FilePath
-- Warning: Takes a lot of assumptions and generally
-- not advised to copy.
--
-- Examples: (output is system dependent)
--   >>> toHaskellFile "My.Module.Lib"
--   "My/Module/Lib.hs"
--   >>> toHaskellFile "Main"
--   "Main.hs"
toHaskellFile :: T.Text -> FilePath
toHaskellFile moduleName = joinPath (map T.unpack $ T.splitOn "." moduleName) <.> ".hs"