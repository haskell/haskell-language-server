-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}

module Development.IDE.Import.FindImports
  ( locateModule
  , Import(..)
  , ArtifactsLocation(..)
  , modSummaryToArtifactsLocation
  , isBootLocation
  , mkImportDirs
  ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.List                         (isSuffixOf)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import qualified Data.Set                          as S
import           Development.IDE.GHC.Compat        as Compat
import           Development.IDE.GHC.Error         as ErrUtils
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           GHC.Types.PkgQual


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

-- | TODO: rewrite documentation
mkImportDirs :: HscEnv -> (UnitId, DynFlags) -> Maybe (UnitId, S.Set ModuleName)
#if MIN_VERSION_ghc(9,11,0)
mkImportDirs _env (i, flags) = Just (i, (S.fromList $ map reexportTo $ reexportedModules flags))
#else
mkImportDirs _env (i, flags) = Just (i, reexportedModules flags)
#endif

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => (Map ModuleName (UnitId, NormalizedFilePath),Map ModuleName (UnitId, NormalizedFilePath))
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
      | Just reexports <- lookup uid reexports_map
          -> lookupLocal uid moduleMaps reexports
      | otherwise -> return $ Left $ notFoundErr env modName $ LookupNotFound []
    -- if a package name is given we only go look for a package
    OtherPkg uid
      | Just reexports <- lookup uid reexports_map
          -> lookupLocal uid moduleMaps reexports
      | otherwise -> lookupInPackageDB
    NoPkgQual ->
      lookupLocal (homeUnitId_ dflags) moduleMaps mempty

      -- Reexports for current unit have to be empty because they only apply to other units depending on the
      -- current unit. If we set the reexports to be the actual reexports then we risk looping forever trying
      -- to find the module from the perspective of the current unit.
      -- [About The reexported module]
      --
      -- A package (or unit) A can reexport a module from another package/unit.
      --
      -- When it happen, it means two things:
      --
      -- - This module must appear in 'moduleMaps', using the correct package/unit
      -- - What about "conflict". Right now the moduleMaps maps a module name to a unique package/unit.
  where
    dflags = hsc_dflags env
    reexports_map = mapMaybe (mkImportDirs env) comp_info

    toModLocation uid file = liftIO $ do
        loc <- mkHomeModLocation dflags (unLoc modName) (fromNormalizedFilePath file)
        let genMod = mkModule (RealUnit $ Definite uid) (unLoc modName)  -- TODO support backpack holes
        return $ Right $ FileImport $ ArtifactsLocation file (Just loc) (not isSource) (Just genMod)

    -- TODO: maybe rename this function, the "local" does not really mean much
    -- TODO: test the support for "reexports". The previous implementation had
    -- an "other_modules" logic which was not ported, because not really
    -- understood.
    lookupLocal currentUid moduleMaps@(moduleMap, moduleMapSource) reexports = do
          let mbFile = case Map.lookup (unLoc modName) (if isSource then moduleMapSource else moduleMap) of
                         Nothing          -> if (unLoc modName) `S.member` reexports
                                             then LocateFoundReexport currentUid
                                             else LocateNotFound
                         Just (uid, file) -> LocateFoundFile uid file
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
