-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP            #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Development.IDE.Import.FindImports
  ( locateModule
  , Import(..)
  , ArtifactsLocation(..)
  , modSummaryToArtifactsLocation
  , isBootLocation
  , ModuleToFilenames(..)
  , mkModuleToFilenames
  , mkReexports
  ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.List                         (find, intercalate,
                                                    isSuffixOf, sortOn)
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.List.NonEmpty                as NE
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (listToMaybe)
import qualified Data.Set                          as S
import           Development.IDE.GHC.Compat        as Compat
import           Development.IDE.GHC.Error         as ErrUtils
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           GHC.Fingerprint
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

-- | For each module name, the units that provide it and the exact location
-- where the unit has it.
-- see 'locateModuleFile' for how we decide which unit an import actually
-- resolves to.
data ModuleToFilenames = ModuleToFilenames {
  -- | "normal" files (e.g. @.hs@)
  moduleMap       :: UniqMap ModuleName (NonEmpty (UnitId, NormalizedFilePath)),
  -- | "boot" files (e.g. @.hs-boot@)
  moduleMapSource :: UniqMap ModuleName (NonEmpty (UnitId, NormalizedFilePath)),
  -- | Fingerprint of the two maps, for early cutoff
  mtfFingerprint  :: !Fingerprint
}
  deriving Generic

-- | The fingerprint is strict and computing it forces both maps, so there is
-- nothing left to force. Deep forcing here would traverse the maps again for
-- every file of the session.
instance NFData ModuleToFilenames where
  rnf = rwhnf

instance Show ModuleToFilenames where
  show mtf = "ModuleToFilenames " ++ show (mtfFingerprint mtf)

mkModuleToFilenames
    :: UniqMap ModuleName (NonEmpty (UnitId, NormalizedFilePath))
    -> UniqMap ModuleName (NonEmpty (UnitId, NormalizedFilePath))
    -> ModuleToFilenames
mkModuleToFilenames normal source =
    ModuleToFilenames normal source (fingerprintFingerprints [fp normal, fp source])
  where
    fp m = fingerprintFingerprints
      [ fingerprintString $ intercalate "\0" $
          moduleNameString mn :
          concat [ [unitIdString u, fromNormalizedFilePath p] | (u, p) <- NE.toList provs ]
      | (mn, provs) <- sortOn (moduleNameString . fst) (nonDetEltsUFM (getUniqMap m))
      ]

data LocateResult
  = LocateNotFound
  | LocateFoundReexport UnitId
  | LocateFoundFile UnitId NormalizedFilePath

-- | Locate a module in the file system.
--
-- Only the given units are searched, in the given order, and we pick the
-- first one providing the module. A unit outside the list is not visible
-- here.
locateModuleFile
  :: ModuleToFilenames
  -> Bool
  -> ModuleName
  -> [(UnitId, S.Set ModuleName)] -- ^ units to search, in priority order, with their reexports
  -> LocateResult
locateModuleFile ModuleToFilenames{moduleMap, moduleMapSource} isSource modName unit_reexports =
  case mbFile of
    Just (uid, file) -> LocateFoundFile uid file
    Nothing ->
      case find (\(_ , reexports) -> S.member modName reexports) unit_reexports of
        Just (uid,_) -> LocateFoundReexport uid
        Nothing      -> LocateNotFound
  where
    providers = maybe [] NE.toList $
      lookupUniqMap (if isSource then moduleMapSource else moduleMap) modName
    mbFile = listToMaybe
      [ (uid, file) | (uid, _) <- unit_reexports, Just file <- [lookup uid providers] ]

-- | This function is used to map a package name to a set of reexports.
mkReexports :: (UnitId, DynFlags) -> (UnitId, (S.Set ModuleName))
#if MIN_VERSION_ghc(9,11,0)
mkReexports (i, flags) = (i, (S.fromList $ map reexportTo $ reexportedModules flags))
#else
mkReexports (i, flags) = (i, (reexportedModules flags))
#endif

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => ModuleToFilenames
    -> HscEnv
    -> Map UnitId (S.Set ModuleName)   -- ^ Reexported modules of each home unit
    -> Located ModuleName              -- ^ Module name
    -> PkgQual                -- ^ Package name
    -> Bool                            -- ^ Is boot module
    -> m (Either [FileDiagnostic] Import)
locateModule moduleMaps env unit_reexports modName mbPkgName isSource = do
  case mbPkgName of
    -- 'ThisPkg' just means some home module, not the current unit
    ThisPkg uid
      | Just reexports <- Map.lookup uid unit_reexports
          -> lookupLocal uid reexports
      | otherwise -> return $ Left $ notFoundErr env modName $ LookupNotFound []
    -- if a package name is given we only go look for a package
    OtherPkg uid
      | Just reexports <- Map.lookup uid unit_reexports
          -> lookupLocal uid reexports
      | otherwise -> lookupInPackageDB
    NoPkgQual -> do
      let mbFile = locateModuleFile moduleMaps isSource (unLoc modName) searchUnits
      case mbFile of
        LocateNotFound -> lookupInPackageDB
        -- Lookup again with the perspective of the unit reexporting the file
        LocateFoundReexport uid -> locateModule moduleMaps (hscSetActiveUnitId uid env) unit_reexports modName noPkgQual isSource
        LocateFoundFile uid file -> toModLocation uid file
  where
    dflags = hsc_dflags env

    -- The units an unqualified import may come from: the current unit first,
    -- then its dependencies, in the given order, which decides who wins when
    -- several provide the module.
    -- The current unit's reexports have to be empty because they only apply to
    -- units depending on it, and using them here would loop, looking for the
    -- module from the perspective of the unit we are already in.
    searchUnits = (homeUnitId_ dflags, S.empty) :
      [ (uid, reexports)
      | uid <- hpt_deps
      , Just reexports <- [Map.lookup uid unit_reexports]
      ]

    ue = hsc_unit_env env
    units = homeUnitEnv_units $ ue_findHomeUnitEnv (homeUnitId_ dflags) ue
    hpt_deps :: [UnitId]
    hpt_deps = homeUnitDepends units

    toModLocation uid file = liftIO $ do
        loc <- mkHomeModLocation dflags (unLoc modName) (fromNormalizedFilePath file)
        let genMod = mkModule (RealUnit $ Definite uid) (unLoc modName)  -- TODO support backpack holes
        return $ Right $ FileImport $ ArtifactsLocation file (Just loc) (not isSource) (Just genMod)

    lookupLocal uid reexports = do
      let mbFile = locateModuleFile moduleMaps isSource (unLoc modName) [(uid, reexports)]
      case mbFile of
        LocateNotFound -> return $ Left $ notFoundErr env modName $ LookupNotFound []
        -- Lookup again with the perspective of the unit reexporting the file
        LocateFoundReexport uid' -> locateModule moduleMaps (hscSetActiveUnitId uid' env) unit_reexports modName noPkgQual isSource
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
