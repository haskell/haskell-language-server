-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}

module Development.IDE.Import.FindImports
  ( locateModule
  , Import(..)
  ) where

import           Development.IDE.GHC.Error as ErrUtils
import Development.IDE.GHC.Orphans()
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
-- GHC imports
import           DynFlags
import           FastString
import           GHC
import qualified Module                      as M
import           Packages
import           Outputable                  (showSDoc, ppr, pprPanic)
import           Finder
import Control.DeepSeq

-- standard imports
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           System.FilePath

data Import
  = FileImport !NormalizedFilePath
  | PackageImport !M.InstalledUnitId
  deriving (Show)

instance NFData Import where
  rnf (FileImport x) = rnf x
  rnf (PackageImport x) = rnf x


-- | locate a module in the file system. Where we go from *daml to Haskell
locateModuleFile :: MonadIO m
             => DynFlags
             -> [String]
             -> (NormalizedFilePath -> m Bool)
             -> ModuleName
             -> m (Maybe NormalizedFilePath)
locateModuleFile dflags exts doesExist modName = do
  let candidates = [ toNormalizedFilePath (prefix </> M.moduleNameSlashes modName <.> ext) | prefix <- importPaths dflags, ext <- exts]
  findM doesExist candidates

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => DynFlags
    -> [String]
    -> (NormalizedFilePath -> m Bool)
    -> Located ModuleName
    -> Maybe FastString
    -> m (Either [FileDiagnostic] Import)
locateModule dflags exts doesExist modName mbPkgName = do
  case mbPkgName of
    -- if a package name is given we only go look for a package
    Just _pkgName -> lookupInPackageDB dflags
    Nothing -> do
      -- first try to find the module as a file. If we can't find it try to find it in the package
      -- database.
      mbFile <- locateModuleFile dflags exts doesExist $ unLoc modName
      case mbFile of
        Nothing -> lookupInPackageDB dflags
        Just file -> return $ Right $ FileImport file
  where
    lookupInPackageDB dfs =
      case lookupModuleWithSuggestions dfs (unLoc modName) mbPkgName of
        LookupFound _m pkgConfig -> return $ Right $ PackageImport $ unitId pkgConfig
        reason -> return $ Left $ notFoundErr dfs modName reason

-- | Don't call this on a found module.
notFoundErr :: DynFlags -> Located M.ModuleName -> LookupResult -> [FileDiagnostic]
notFoundErr dfs modName reason =
  mkError' $ ppr' $ cannotFindModule dfs modName0 $ lookupToFindResult reason
  where
    mkError' = diagFromString "not found" (getLoc modName)
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
#if __GLASGOW_HASKELL__ >= 806
        LookupUnusable unusable ->
          let unusables' = map get_unusable unusable
              get_unusable (m, ModUnusable r) = (moduleUnitId m, r)
              get_unusable (_, r) =
                pprPanic "findLookupResult: unexpected origin" (ppr r)
           in notFound {fr_unusables = unusables'}
#endif
        LookupNotFound suggest ->
          notFound {fr_suggestions = suggest}

notFound :: FindResult
notFound = NotFound
  { fr_paths = []
  , fr_pkg = Nothing
  , fr_pkgs_hidden = []
  , fr_mods_hidden = []
#if __GLASGOW_HASKELL__ >= 806
  , fr_unusables = []
#endif
  , fr_suggestions = []
  }
