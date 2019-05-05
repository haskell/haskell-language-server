-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module Development.IDE.Functions.FindImports
  ( getImportsParsed
  , locateModule
  , Import(..)
  ) where

import           Development.IDE.Functions.GHCError as ErrUtils

-- GHC imports
import           BasicTypes                  (StringLiteral(..))
import           DynFlags
import           FastString
import           GHC
import qualified HeaderInfo                  as Hdr
import qualified Module                      as M
import qualified GHC.LanguageExtensions.Type as GHC
import           Packages
import           Outputable                  (showSDoc, ppr, pprPanic)
import           Finder

-- standard imports
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.Except            as Ex
import           System.FilePath

data Import
  = FileImport FilePath
  | PackageImport M.InstalledUnitId
  deriving (Show)

-- | GhcMonad function to chase imports of a module given as a StringBuffer. Returns given module's
-- name and its imports.
getImportsParsed :: Monad m =>
               DynFlags ->
               GHC.ParsedSource ->
               Ex.ExceptT [FileDiagnostic] m
                          (M.ModuleName, [(Maybe FastString, Located M.ModuleName)])
getImportsParsed dflags (L loc parsed) = do
  let modName = maybe (GHC.mkModuleName "Main") GHC.unLoc $ GHC.hsmodName parsed

  -- refuse source imports
  let srcImports = filter (ideclSource . GHC.unLoc) $ GHC.hsmodImports parsed
  when (not $ null srcImports) $ Ex.throwE $
    concat
      [ mkErrors dflags [(mloc, "Illegal source import of " <> GHC.moduleNameString (GHC.unLoc $ GHC.ideclName i))]
      | L mloc i <- srcImports ]

  -- most of these corner cases are also present in https://hackage.haskell.org/package/ghc-8.6.1/docs/src/HeaderInfo.html#getImports
  -- but we want to avoid parsing the module twice
  let implicit_prelude = xopt GHC.ImplicitPrelude dflags
      implicit_imports = Hdr.mkPrelImports modName loc implicit_prelude $ GHC.hsmodImports parsed

  -- filter out imports that come from packages
  return (modName, [(fmap sl_fs $ ideclPkgQual i, ideclName i)
    | i <- map GHC.unLoc $ implicit_imports ++ GHC.hsmodImports parsed
    , GHC.moduleNameString (GHC.unLoc $ ideclName i) /= "GHC.Prim"
    ])


-- | locate a module in the file system. Where we go from *daml to Haskell
locateModuleFile :: MonadIO m
             => DynFlags
             -> (FilePath -> m Bool)
             -> ModuleName
             -> m (Maybe FilePath)
locateModuleFile dflags doesExist modName = do
  let libPaths = importPaths dflags
  let candidates = [ prefix </> M.moduleNameSlashes modName <.> "daml" | prefix <- libPaths ]
  findM doesExist candidates

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => DynFlags
    -> (FilePath -> m Bool)
    -> Located ModuleName
    -> Maybe FastString
    -> m (Either [FileDiagnostic] Import)
locateModule dflags doesExist modName mbPkgName = do
  case mbPkgName of
    -- if a package name is given we only go look for a package
    Just _pkgName -> lookupInPackageDB dflags
    Nothing -> do
      -- first try to find the module as a file. If we can't find it try to find it in the package
      -- database.
      mbFile <- locateModuleFile dflags doesExist $ unLoc modName
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
    mkError' = mkError dfs (getLoc modName)
    modName0 = unLoc modName
    ppr' = showSDoc dfs
    -- We convert the lookup result to a find result to reuse GHC's cannotFindMoudle pretty printer.
    lookupToFindResult =
      \case
        LookupFound _m _pkgConfig ->
          pprPanic "Impossible: called lookupToFind on found module." (ppr modName0)
        LookupMultiple rs -> (FoundMultiple rs)
        LookupHidden pkg_hiddens mod_hiddens ->
          (NotFound
             { fr_paths = []
             , fr_pkg = Nothing
             , fr_pkgs_hidden = map (moduleUnitId . fst) pkg_hiddens
             , fr_mods_hidden = map (moduleUnitId . fst) mod_hiddens
             , fr_unusables = []
             , fr_suggestions = []
             })
        LookupUnusable unusable ->
          let unusables' = map get_unusable unusable
              get_unusable (m, ModUnusable r) = (moduleUnitId m, r)
              get_unusable (_, r) =
                pprPanic "findLookupResult: unexpected origin" (ppr r)
           in (NotFound
                 { fr_paths = []
                 , fr_pkg = Nothing
                 , fr_pkgs_hidden = []
                 , fr_mods_hidden = []
                 , fr_unusables = unusables'
                 , fr_suggestions = []
                 })
        LookupNotFound suggest ->
          (NotFound
             { fr_paths = []
             , fr_pkg = Nothing
             , fr_pkgs_hidden = []
             , fr_mods_hidden = []
             , fr_unusables = []
             , fr_suggestions = suggest
             })
