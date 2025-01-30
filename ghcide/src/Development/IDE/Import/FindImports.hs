-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}

module Development.IDE.Import.FindImports
  ( locateModule
  , locateModuleFile
  , Import(..)
  , ArtifactsLocation(..)
  , modSummaryToArtifactsLocation
  , isBootLocation
  , mkImportDirs
  ) where

import           Control.DeepSeq
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Data.List                         (find, isSuffixOf)
import           Data.Maybe
import qualified Data.Set                          as S
import           Development.IDE.GHC.Compat        as Compat
import           Development.IDE.GHC.Error         as ErrUtils
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           GHC.Types.PkgQual
import           GHC.Unit.State
import           System.FilePath


data Import
  = FileImport !ArtifactsLocation
  | PackageImport
  deriving (Show)

data ArtifactsLocation = ArtifactsLocation
  { artifactFilePath    :: !NormalizedFilePath
  , artifactModLocation :: !(Maybe ModLocation)
  , artifactIsSource    :: !Bool          -- ^ True if a module is a source input
  , artifactModule      :: !(Maybe Module)
  } deriving Show

instance NFData ArtifactsLocation where
  rnf ArtifactsLocation{..} = rnf artifactFilePath `seq` rwhnf artifactModLocation `seq` rnf artifactIsSource `seq` rnf artifactModule

isBootLocation :: ArtifactsLocation -> Bool
isBootLocation = not . artifactIsSource

instance NFData Import where
  rnf (FileImport x) = rnf x
  rnf PackageImport  = ()

modSummaryToArtifactsLocation :: NormalizedFilePath -> Maybe ModSummary -> ArtifactsLocation
modSummaryToArtifactsLocation nfp ms = ArtifactsLocation nfp (ms_location <$> ms) source mbMod
  where
    isSource HsSrcFile = True
    isSource _         = False
    source = case ms of
      Nothing     -> "-boot" `isSuffixOf` fromNormalizedFilePath nfp
      Just modSum -> isSource (ms_hsc_src modSum)
    mbMod = ms_mod <$> ms

data LocateResult
  = LocateNotFound
  | LocateFoundReexport UnitId
  | LocateFoundFile UnitId NormalizedFilePath

-- | locate a module in the file system. Where we go from *daml to Haskell
locateModuleFile :: MonadIO m
             => [(UnitId, [FilePath], S.Set ModuleName)]
             -> [String]
             -> (ModuleName -> NormalizedFilePath -> m (Maybe NormalizedFilePath))
             -> Bool
             -> ModuleName
             -> m LocateResult
locateModuleFile import_dirss exts targetFor isSource modName = do
  let candidates import_dirs =
        [ toNormalizedFilePath' (prefix </> moduleNameSlashes modName <.> maybeBoot ext)
           | prefix <- import_dirs , ext <- exts]
  mf <- firstJustM go (concat [map (uid,) (candidates dirs) | (uid, dirs, _) <- import_dirss])
  case mf of
    Nothing ->
      case find (\(_ , _, reexports) -> S.member modName reexports) import_dirss of
        Just (uid,_,_) -> pure $ LocateFoundReexport uid
        Nothing        -> pure LocateNotFound
    Just (uid,file) -> pure $ LocateFoundFile uid file
  where
    go (uid, candidate) = fmap ((uid,) <$>) $ targetFor modName candidate
    maybeBoot ext
      | isSource = ext ++ "-boot"
      | otherwise = ext

-- | This function is used to map a package name to a set of import paths.
-- It only returns Just for unit-ids which are possible to import into the
-- current module. In particular, it will return Nothing for 'main' components
-- as they can never be imported into another package.
mkImportDirs :: HscEnv -> (UnitId, DynFlags) -> Maybe (UnitId, ([FilePath], S.Set ModuleName))
mkImportDirs _env (i, flags) = Just (i, (importPaths flags, reexportedModules flags))

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => HscEnv
    -> [(UnitId, DynFlags)] -- ^ Import directories
    -> [String]                        -- ^ File extensions
    -> (ModuleName -> NormalizedFilePath -> m (Maybe NormalizedFilePath))  -- ^ does file exist predicate
    -> Located ModuleName              -- ^ Module name
    -> PkgQual                -- ^ Package name
    -> Bool                            -- ^ Is boot module
    -> m (Either [FileDiagnostic] Import)
locateModule env comp_info exts targetFor modName mbPkgName isSource = do
  case mbPkgName of
    -- 'ThisPkg' just means some home module, not the current unit
    ThisPkg uid
      | Just (dirs, reexports) <- lookup uid import_paths
          -> lookupLocal uid dirs reexports
      | otherwise -> return $ Left $ notFoundErr env modName $ LookupNotFound []
    -- if a package name is given we only go look for a package
    OtherPkg uid
      | Just (dirs, reexports) <- lookup uid import_paths
          -> lookupLocal uid dirs reexports
      | otherwise -> lookupInPackageDB
    NoPkgQual -> do

      -- Reexports for current unit have to be empty because they only apply to other units depending on the
      -- current unit. If we set the reexports to be the actual reexports then we risk looping forever trying
      -- to find the module from the perspective of the current unit.
      mbFile <- locateModuleFile ((homeUnitId_ dflags, importPaths dflags, S.empty) : other_imports) exts targetFor isSource $ unLoc modName
      case mbFile of
        LocateNotFound -> lookupInPackageDB
        -- Lookup again with the perspective of the unit reexporting the file
        LocateFoundReexport uid -> locateModule (hscSetActiveUnitId uid env) comp_info exts targetFor modName noPkgQual isSource
        LocateFoundFile uid file -> toModLocation uid file
  where
    dflags = hsc_dflags env
    import_paths = mapMaybe (mkImportDirs env) comp_info
    other_imports =
#if MIN_VERSION_ghc(9,4,0)
      -- On 9.4+ instead of bringing all the units into scope, only bring into scope the units
      -- this one depends on
      -- This way if you have multiple units with the same module names, we won't get confused
      -- For example if unit a imports module M from unit B, when there is also a module M in unit C,
      -- and unit a only depends on unit b, without this logic there is the potential to get confused
      -- about which module unit a imports.
      -- Without multi-component support it is hard to recontruct the dependency environment so
      -- unit a will have both unit b and unit c in scope.
      map (\uid -> let this_df = homeUnitEnv_dflags (ue_findHomeUnitEnv uid ue) in (uid, importPaths this_df, reexportedModules this_df)) hpt_deps
    ue = hsc_unit_env env
    units = homeUnitEnv_units $ ue_findHomeUnitEnv (homeUnitId_ dflags) ue
    hpt_deps :: [UnitId]
    hpt_deps = homeUnitDepends units
#else
      _import_paths'
#endif

      -- first try to find the module as a file. If we can't find it try to find it in the package
      -- database.
      -- Here the importPaths for the current modules are added to the front of the import paths from the other components.
      -- This is particularly important for Paths_* modules which get generated for every component but unless you use it in
      -- each component will end up being found in the wrong place and cause a multi-cradle match failure.
    _import_paths' = -- import_paths' is only used in GHC < 9.4
            import_paths

    toModLocation uid file = liftIO $ do
        loc <- mkHomeModLocation dflags (unLoc modName) (fromNormalizedFilePath file)
        let genMod = mkModule (RealUnit $ Definite uid) (unLoc modName)  -- TODO support backpack holes
        return $ Right $ FileImport $ ArtifactsLocation file (Just loc) (not isSource) (Just genMod)

    lookupLocal uid dirs reexports = do
      mbFile <- locateModuleFile [(uid, dirs, reexports)] exts targetFor isSource $ unLoc modName
      case mbFile of
        LocateNotFound -> return $ Left $ notFoundErr env modName $ LookupNotFound []
        -- Lookup again with the perspective of the unit reexporting the file
        LocateFoundReexport uid' -> locateModule (hscSetActiveUnitId uid' env) comp_info exts targetFor modName noPkgQual isSource
        LocateFoundFile uid' file -> toModLocation uid' file

    lookupInPackageDB = do
      case Compat.lookupModuleWithSuggestions env (unLoc modName) mbPkgName of
        LookupFound _m _pkgConfig -> return $ Right PackageImport
        reason -> return $ Left $ notFoundErr env modName reason

-- | Don't call this on a found module.
notFoundErr :: HscEnv -> Located ModuleName -> LookupResult -> [FileDiagnostic]
notFoundErr env modName reason =
  mkError' $ ppr' $ cannotFindModule env modName0 $ lookupToFindResult reason
  where
    dfs = hsc_dflags env
    mkError' doc = diagFromString "not found" DiagnosticSeverity_Error (Compat.getLoc modName) doc Nothing
    modName0 = unLoc modName
    ppr' = showSDoc dfs
    -- We convert the lookup result to a find result to reuse GHC's cannotFindModule pretty printer.
    lookupToFindResult =
      \case
        LookupFound _m _pkgConfig ->
          pprPanic "Impossible: called lookupToFind on found module." (ppr modName0)
        LookupMultiple rs -> FoundMultiple rs
        LookupHidden pkg_hiddens mod_hiddens ->
          notFound
             { fr_pkgs_hidden = map (moduleUnit . fst) pkg_hiddens
             , fr_mods_hidden = map (moduleUnit . fst) mod_hiddens
             }
        LookupUnusable unusable ->
          let unusables' = map get_unusable unusable
#if MIN_VERSION_ghc(9,6,4) && (!MIN_VERSION_ghc(9,8,1) || MIN_VERSION_ghc(9,8,2))
              get_unusable (_m, ModUnusable r) = r
#else
              get_unusable (m, ModUnusable r) = (moduleUnit m, r)
#endif
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

noPkgQual :: PkgQual
noPkgQual = NoPkgQual
