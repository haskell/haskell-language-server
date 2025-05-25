-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}

module Development.IDE.Import.FindImports
  ( locateModule
  , Import(..)
  , ArtifactsLocation(..)
  , modSummaryToArtifactsLocation
  , isBootLocation
  , ModuleToFilenames(..)
  ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.List                         (find, isSuffixOf)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Set                          as S
import           Development.IDE.GHC.Compat        as Compat
import           Development.IDE.GHC.Error         as ErrUtils
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           GHC.Generics
import           GHC.Types.PkgQual
import           GHC.Unit


#if MIN_VERSION_ghc(9,11,0)
import           GHC.Driver.DynFlags
#endif

data Import
  = FileImport !ArtifactsLocation
  | PackageImport
  deriving (Show)

data ArtifactsLocation = ArtifactsLocation
  { artifactFilePath    :: !NormalizedFilePath
  , artifactModLocation :: !(Maybe ModLocation)
  , artifactIsSource    :: !Bool          -- ^ 'True' for a real Haskell source file ('HsSrcFile');
                                          -- 'False' for a boot ('HsBootFile') or signature ('HsigFile') file.
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

-- | Represents a mapping from module name to the associated unit and filepath
data ModuleToFilenames = ModuleToFilenames {
  -- | "normal" files (e.g. @.hs@)
  moduleMap       :: Map ModuleName (UnitId, NormalizedFilePath),
  -- | "boot" files (e.g. @.hs-boot@)
  moduleMapSource :: Map ModuleName (UnitId, NormalizedFilePath)
}
  deriving (Show, NFData, Generic)

-- | The mapping of one module to a file takes precedence in "import path"
-- order
instance Semigroup ModuleToFilenames where
  ModuleToFilenames a b <> ModuleToFilenames a' b' = ModuleToFilenames (a <> a') (b <> b')

instance Monoid ModuleToFilenames where
  mempty = ModuleToFilenames mempty mempty

data LocateResult
  = LocateNotFound
  | LocateFoundReexport UnitId
  | LocateFoundFile UnitId NormalizedFilePath

-- | locate a module in the file system. Where we go from *daml to Haskell
locateModuleFile :: ModuleToFilenames -> Bool -> ModuleName -> [(UnitId, S.Set ModuleName)] -> LocateResult
locateModuleFile ModuleToFilenames{..} isSource modName uid_reexports = do
  case Map.lookup modName (if isSource then moduleMapSource else moduleMap) of
                 Nothing ->
                   case find (\(_ , reexports) -> S.member modName reexports) uid_reexports of
                           Just (uid,_) -> LocateFoundReexport uid
                           Nothing      -> LocateNotFound
                 Just (uid, file) -> LocateFoundFile uid file

-- | This function is used to map a package name to a set of reexports
-- TODO: the rest of this comment seems outdated:
-- It only returns Just for unit-ids which are possible to import into the
-- current module. In particular, it will return Nothing for 'main' components
-- as they can never be imported into another package.
mkReexports :: HscEnv -> (UnitId, DynFlags) -> Maybe (UnitId, (S.Set ModuleName))
#if MIN_VERSION_ghc(9,11,0)
mkReexports _env (i, flags) = Just (i, (S.fromList $ map reexportTo $ reexportedModules flags))
#else
mkReexports _env (i, flags) = Just (i, (reexportedModules flags))
#endif

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => ModuleToFilenames
    -> HscEnv
    -> [(UnitId, DynFlags)] -- ^ Import directories
    -> [String]                        -- ^ File extensions
    -> Located ModuleName              -- ^ Module name
    -> PkgQual                -- ^ Package name
    -> Bool                            -- ^ Is boot module
    -> m (Either [FileDiagnostic] Import)
locateModule moduleMaps env comp_info exts modName mbPkgName isSource = do
  case mbPkgName of
    -- 'ThisPkg' just means some home module, not the current unit
    ThisPkg uid
      -- TODO: there are MANY lookup on import_paths, which is a problem considering that it can be large.
      | Just reexports <- lookup uid import_paths
          -> lookupLocal uid moduleMaps reexports
      | otherwise -> return $ Left $ notFoundErr env modName $ LookupNotFound []
    -- if a package name is given we only go look for a package
    OtherPkg uid
      | Just reexports <- lookup uid import_paths
          -> lookupLocal uid moduleMaps reexports
      | otherwise -> lookupInPackageDB
    NoPkgQual -> do

      -- Reexports for current unit have to be empty because they only apply to other units depending on the
      -- current unit. If we set the reexports to be the actual reexports then we risk looping forever trying
      -- to find the module from the perspective of the current unit.
      let reexports = other_imports
      let mbFile = locateModuleFile moduleMaps isSource (unLoc modName) reexports

      case mbFile of
        LocateNotFound -> lookupInPackageDB
        -- Lookup again with the perspective of the unit reexporting the file
        LocateFoundReexport uid -> locateModule moduleMaps (hscSetActiveUnitId uid env) comp_info exts modName noPkgQual isSource
        LocateFoundFile uid file -> toModLocation uid file
  where
    dflags = hsc_dflags env
    import_paths = mapMaybe (mkReexports env) comp_info

    other_imports =
      -- Instead of bringing all the units into scope, only bring into scope the units
      -- this one depends on.
      -- This way if you have multiple units with the same module names, we won't get confused
      -- For example if unit a imports module M from unit B, when there is also a module M in unit C,
      -- and unit a only depends on unit b, without this logic there is the potential to get confused
      -- about which module unit a imports.
      -- Without multi-component support it is hard to recontruct the dependency environment so
      -- unit a will have both unit b and unit c in scope.
#if MIN_VERSION_ghc(9,11,0)
      map (\uid -> let this_df = homeUnitEnv_dflags (ue_findHomeUnitEnv uid ue) in (uid, S.fromList $ map reexportTo $ reexportedModules this_df)) hpt_deps
#else
      map (\uid -> let this_df = homeUnitEnv_dflags (ue_findHomeUnitEnv uid ue) in (uid, reexportedModules this_df)) hpt_deps
#endif
    ue = hsc_unit_env env
    units = homeUnitEnv_units $ ue_findHomeUnitEnv (homeUnitId_ dflags) ue
    hpt_deps :: [UnitId]
    hpt_deps = homeUnitDepends units

    toModLocation uid file = liftIO $ do
        loc <- mkHomeModLocation dflags (unLoc modName) (fromNormalizedFilePath file)
        let genMod = mkModule (RealUnit $ Definite uid) (unLoc modName)  -- TODO support backpack holes
        return $ Right $ FileImport $ ArtifactsLocation file (Just loc) (not isSource) (Just genMod)

    lookupLocal uid moduleMaps reexports = do
      let mbFile = locateModuleFile moduleMaps isSource (unLoc modName) [(uid, reexports)]
      case mbFile of
        LocateNotFound -> return $ Left $ notFoundErr env modName $ LookupNotFound []
        -- Lookup again with the perspective of the unit reexporting the file
        LocateFoundReexport uid' -> locateModule moduleMaps (hscSetActiveUnitId uid' env) comp_info exts modName noPkgQual isSource
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
