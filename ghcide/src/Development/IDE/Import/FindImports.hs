-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Development.IDE.Import.FindImports
  ( locateModule
  , Import(..)
  , ArtifactsLocation(..)
  , modSummaryToArtifactsLocation
  , isBootLocation
  , mkImportDirs
  ) where

import           Development.IDE.GHC.Error as ErrUtils
import Development.IDE.GHC.Orphans()
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.GHC.Compat
-- GHC imports
import           FastString
import qualified Module                      as M
import           Packages
import           Outputable                  (showSDoc, ppr, pprPanic)
import           Finder
import Control.DeepSeq

-- standard imports
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           System.FilePath
import DriverPhases
import Data.Maybe
import Data.List (isSuffixOf)

data Import
  = FileImport !ArtifactsLocation
  | PackageImport !M.InstalledUnitId
  deriving (Show)

data ArtifactsLocation = ArtifactsLocation
  { artifactFilePath    :: !NormalizedFilePath
  , artifactModLocation :: !(Maybe ModLocation)
  , artifactIsSource    :: !Bool          -- ^ True if a module is a source input
  }
    deriving (Show)

instance NFData ArtifactsLocation where
  rnf ArtifactsLocation{..} = rnf artifactFilePath `seq` rwhnf artifactModLocation `seq` rnf artifactIsSource

isBootLocation :: ArtifactsLocation -> Bool
isBootLocation = not . artifactIsSource

instance NFData Import where
  rnf (FileImport x) = rnf x
  rnf (PackageImport x) = rnf x

modSummaryToArtifactsLocation :: NormalizedFilePath -> Maybe ModSummary -> ArtifactsLocation
modSummaryToArtifactsLocation nfp ms = ArtifactsLocation nfp (ms_location <$> ms) source
  where
    isSource HsSrcFile = True
    isSource _ = False
    source = case ms of
      Nothing -> "-boot" `isSuffixOf` fromNormalizedFilePath nfp
      Just ms -> isSource (ms_hsc_src ms)

-- | locate a module in the file system. Where we go from *daml to Haskell
locateModuleFile :: MonadIO m
             => [[FilePath]]
             -> [String]
             -> (ModuleName -> NormalizedFilePath -> m Bool)
             -> Bool
             -> ModuleName
             -> m (Maybe NormalizedFilePath)
locateModuleFile import_dirss exts doesExist isSource modName = do
  let candidates import_dirs =
        [ toNormalizedFilePath' (prefix </> M.moduleNameSlashes modName <.> maybeBoot ext)
           | prefix <- import_dirs , ext <- exts]
  findM (doesExist modName) (concatMap candidates import_dirss)
  where
    maybeBoot ext
      | isSource = ext ++ "-boot"
      | otherwise = ext

-- | This function is used to map a package name to a set of import paths.
-- It only returns Just for unit-ids which are possible to import into the
-- current module. In particular, it will return Nothing for 'main' components
-- as they can never be imported into another package.
mkImportDirs :: DynFlags -> (M.InstalledUnitId, DynFlags) -> Maybe (PackageName, [FilePath])
mkImportDirs df (i, DynFlags{importPaths}) = (, importPaths) <$> getPackageName df i

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => DynFlags
    -> [(M.InstalledUnitId, DynFlags)] -- ^ Import directories
    -> [String]                        -- ^ File extensions
    -> (ModuleName -> NormalizedFilePath -> m Bool)  -- ^ does file exist predicate
    -> Located ModuleName              -- ^ Moudle name
    -> Maybe FastString                -- ^ Package name
    -> Bool                            -- ^ Is boot module
    -> m (Either [FileDiagnostic] Import)
locateModule dflags comp_info exts doesExist modName mbPkgName isSource = do
  case mbPkgName of
    -- "this" means that we should only look in the current package
    Just "this" -> do
      lookupLocal [importPaths dflags]
    -- if a package name is given we only go look for a package
    Just pkgName
      | Just dirs <- lookup (PackageName pkgName) import_paths
          -> lookupLocal [dirs]
      | otherwise -> lookupInPackageDB dflags
    Nothing -> do
      -- first try to find the module as a file. If we can't find it try to find it in the package
      -- database.
      -- Here the importPaths for the current modules are added to the front of the import paths from the other components.
      -- This is particularly important for Paths_* modules which get generated for every component but unless you use it in
      -- each component will end up being found in the wrong place and cause a multi-cradle match failure.
      mbFile <- locateModuleFile (importPaths dflags : map snd import_paths) exts doesExist isSource $ unLoc modName
      case mbFile of
        Nothing -> lookupInPackageDB dflags
        Just file -> toModLocation file
  where
    import_paths = mapMaybe (mkImportDirs dflags) comp_info
    toModLocation file = liftIO $ do
        loc <- mkHomeModLocation dflags (unLoc modName) (fromNormalizedFilePath file)
        return $ Right $ FileImport $ ArtifactsLocation file (Just loc) (not isSource)

    lookupLocal dirs = do
      mbFile <- locateModuleFile dirs exts doesExist isSource $ unLoc modName
      case mbFile of
        Nothing -> return $ Left $ notFoundErr dflags modName $ LookupNotFound []
        Just file -> toModLocation file

    lookupInPackageDB dfs =
      case lookupModuleWithSuggestions dfs (unLoc modName) mbPkgName of
        LookupFound _m pkgConfig -> return $ Right $ PackageImport $ unitId pkgConfig
        reason -> return $ Left $ notFoundErr dfs modName reason

-- | Don't call this on a found module.
notFoundErr :: DynFlags -> Located M.ModuleName -> LookupResult -> [FileDiagnostic]
notFoundErr dfs modName reason =
  mkError' $ ppr' $ cannotFindModule dfs modName0 $ lookupToFindResult reason
  where
    mkError' = diagFromString "not found" DsError (getLoc modName)
    modName0 = unLoc modName
    ppr' = showSDoc dfs
    -- We convert the lookup result to a find result to reuse GHC's cannotFindMoudle pretty printer.
    lookupToFindResult =
      \case
        LookupFound _m _pkgConfig ->
          pprPanic "Impossible: called lookupToFind on found module." (ppr modName0)
        LookupMultiple rs -> FoundMultiple rs
        LookupHidden pkg_hiddens mod_hiddens ->
          notFound
             { fr_pkgs_hidden = map (moduleUnitId . fst) pkg_hiddens
             , fr_mods_hidden = map (moduleUnitId . fst) mod_hiddens
             }
        LookupUnusable unusable ->
          let unusables' = map get_unusable unusable
              get_unusable (m, ModUnusable r) = (moduleUnitId m, r)
              get_unusable (_, r) =
                pprPanic "findLookupResult: unexpected origin" (ppr r)
           in notFound {fr_unusables = unusables'}
        LookupNotFound suggest ->
          notFound {fr_suggestions = suggest}

notFound :: FindResult
notFound = NotFound
  { fr_paths = []
  , fr_pkg = Nothing
  , fr_pkgs_hidden = []
  , fr_mods_hidden = []
  , fr_unusables = []
  , fr_suggestions = []
  }
