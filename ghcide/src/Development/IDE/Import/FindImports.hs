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
  , mkUnitVisibility
  ) where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import qualified Data.HashSet                      as HS
import           Data.List                         (intercalate, isSuffixOf,
                                                    sort, sortOn)
import           Data.List.NonEmpty                (NonEmpty)
import qualified Data.List.NonEmpty                as NE
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
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
import           GHC.Driver.DynFlags               (ReexportedModule (..),
                                                    hiddenModules)
#else
import           GHC.Driver.Session                (hiddenModules)
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
  -- | Modules and the unit, source pairs they correspond to
  moduleMap      :: UniqMap ModuleName (NonEmpty (UnitId, NormalizedFilePath)),
  -- | Boot files we know exist. If you want to check if a boot file exists,
  -- check this field for precisely the -boot file corresponding to the non-boot
  -- file you have already resolved.
  bootFiles      :: HS.HashSet NormalizedFilePath,
  -- | Fingerprint of the two, for early cutoff
  mtfFingerprint :: !Fingerprint
}
  deriving Generic

-- | The fingerprint is strict and computing it forces the map and the set, so
-- there is nothing left to force. Deep forcing here would traverse them again
-- for every file of the session.
instance NFData ModuleToFilenames where
  rnf = rwhnf

instance Show ModuleToFilenames where
  show mtf = "ModuleToFilenames " ++ show (mtfFingerprint mtf)

mkModuleToFilenames
    :: UniqMap ModuleName (NonEmpty (UnitId, NormalizedFilePath))
    -> HS.HashSet NormalizedFilePath
    -> ModuleToFilenames
mkModuleToFilenames normal boots =
    ModuleToFilenames normal boots (fingerprintFingerprints [fpMap normal, fpSet boots])
  where
    fpMap m = fingerprintFingerprints
      [ fingerprintString $ intercalate "\0" $
          moduleNameString mn :
          concat [ [unitIdString u, fromNormalizedFilePath p] | (u, p) <- NE.toList provs ]
      | (mn, provs) <- sortOn (moduleNameString . fst) (nonDetEltsUFM (getUniqMap m))
      ]
    fpSet s = fingerprintString $ intercalate "\0" $
      sort $ map fromNormalizedFilePath $ HS.toList s

data LocateResult
  = LocateNotFound
  | LocateFoundReexport UnitId ModuleName
    -- ^ The unit reexporting the module, and the name it has there
  | LocateFoundFile UnitId NormalizedFilePath

-- | What a home unit exposes to the units depending on it.
data UnitVisibility = UnitVisibility
  { uvReexports :: Map ModuleName ModuleName
    -- ^ The name we import it under, and the name it has in the unit it is
    -- reexported from
  , uvHidden    :: S.Set ModuleName
  }

-- | What a home unit exposes, from its flags.
mkUnitVisibility :: (UnitId, DynFlags) -> (UnitId, UnitVisibility)
mkUnitVisibility (i, flags) = (i, UnitVisibility reexports (hiddenModules flags))
  where
#if MIN_VERSION_ghc(9,11,0)
    -- Earlier entries win, as in 'GHC.Driver.Config.Finder.initFinderOpts'
    reexports = Map.fromList
      [ (reexportTo r, reexportFrom r) | r <- reverse (reexportedModules flags) ]
#else
    reexports = Map.fromSet id (reexportedModules flags)
#endif

-- | Locate a module in the file system.
--
-- We go through the units in the given order and do exactly what GHC's finder
-- does: if the unit reexports the module we start again from that unit, if it
-- hides the module we skip it, and otherwise it provides the module if it has
-- a file for it. A unit that is not in the list is not visible to the importer.
locateModuleFile
  :: ModuleToFilenames
  -> ModuleName
  -> [(UnitId, Maybe UnitVisibility)]
     -- ^ Units to search, in priority order. 'Nothing' for the importing unit,
     -- whose own reexports and hidden modules do not apply to it.
  -> LocateResult
locateModuleFile ModuleToFilenames{moduleMap} modName = go
  where
    providers = maybe [] NE.toList $ lookupUniqMap moduleMap modName

    go [] = LocateNotFound
    go ((uid, mbVisibility) : units)
      | Just vis <- mbVisibility
      , Just realName <- Map.lookup modName (uvReexports vis)
      = LocateFoundReexport uid realName
      | Just vis <- mbVisibility
      , modName `S.member` uvHidden vis
      = go units
      | Just file <- lookup uid providers
      = LocateFoundFile uid file
      | otherwise
      = go units

-- | locate a module in either the file system or the package database. Where we go from *daml to
-- Haskell
locateModule
    :: MonadIO m
    => ModuleToFilenames
    -> HscEnv
    -> Map UnitId UnitVisibility       -- ^ What each home unit exposes
    -> Located ModuleName              -- ^ Module name
    -> PkgQual                -- ^ Package name
    -> Bool                            -- ^ Is boot module
    -> m (Either [FileDiagnostic] Import)
locateModule moduleMaps env unit_visibility modName mbPkgName isSource = do
  case mbPkgName of
    -- 'ThisPkg' just means some home module, not the current unit
    -- A home unit qualifier is not a package, so the package database is not
    -- consulted when the module is not in that unit
    ThisPkg uid
      | uid == homeUnitId_ dflags -> lookupIn moduleNotFound [(uid, Nothing)]
      | Just vis <- Map.lookup uid unit_visibility -> lookupIn moduleNotFound [(uid, Just vis)]
      | otherwise -> moduleNotFound
    -- if a package name is given we only go look for a package
    OtherPkg uid
      | Just vis <- Map.lookup uid unit_visibility -> lookupIn lookupInPackageDB [(uid, Just vis)]
      | otherwise -> lookupInPackageDB
    NoPkgQual -> lookupIn lookupInPackageDB searchUnits
  where
    dflags = hsc_dflags env

    moduleNotFound = return $ Left $ notFoundErr env modName $ LookupNotFound []

    lookupIn onNotFound units =
      case locateModuleFile moduleMaps (unLoc modName) units of
        LocateNotFound -> onNotFound
        -- Look again from the perspective of the unit reexporting the module,
        -- under the name it has there
        LocateFoundReexport uid realName ->
          locateModule moduleMaps (hscSetActiveUnitId uid env) unit_visibility
            (const realName <$> modName) noPkgQual isSource
        LocateFoundFile uid file
          -- The search only ever finds source files. A SOURCE import takes the
          -- boot file next to the source file we found, and fails if there is
          -- none, we do not go looking anywhere else.
          | isSource -> maybe moduleNotFound (toModLocation uid) (bootFile file)
          | otherwise -> toModLocation uid file

    bootFile file
      | boot `HS.member` bootFiles moduleMaps = Just boot
      | otherwise = Nothing
      where boot = toNormalizedFilePath' $ fromNormalizedFilePath file <> "-boot"

    -- The units an unqualified import may come from: the current unit first,
    -- then its dependencies, in the given order, which decides who wins when
    -- several provide the module.
    -- The current unit's own reexports and hidden modules do not apply to it,
    -- which also stops the reexport search from looping.
    searchUnits = (homeUnitId_ dflags, Nothing) :
      [ (uid, Map.lookup uid unit_visibility) | uid <- hpt_deps ]

    ue = hsc_unit_env env
    units = homeUnitEnv_units $ ue_findHomeUnitEnv (homeUnitId_ dflags) ue
    hpt_deps :: [UnitId]
    hpt_deps = homeUnitDepends units

    toModLocation uid file = liftIO $ do
        loc <- mkHomeModLocation dflags (unLoc modName) (fromNormalizedFilePath file)
        let genMod = mkModule (RealUnit $ Definite uid) (unLoc modName)  -- TODO support backpack holes
            loc' = if isSource then addBootSuffixLocnOut loc else loc
        return $ Right $ FileImport $ ArtifactsLocation file (Just loc') (not isSource) (Just genMod)

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
