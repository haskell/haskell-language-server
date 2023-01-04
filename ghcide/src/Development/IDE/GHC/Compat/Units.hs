{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compat module for 'UnitState' and 'UnitInfo'.
module Development.IDE.GHC.Compat.Units (
    -- * UnitState
    UnitState,
    initUnits,
    oldInitUnits,
    unitState,
    getUnitName,
    explicitUnits,
    preloadClosureUs,
    listVisibleModuleNames,
    LookupResult(..),
    lookupModuleWithSuggestions,
    -- * UnitInfoMap
    UnitInfoMap,
    getUnitInfoMap,
    lookupUnit,
    lookupUnit',
    -- * UnitInfo
    UnitInfo,
    unitExposedModules,
    unitDepends,
    unitHaddockInterfaces,
    unitInfoId,
    unitPackageNameString,
    unitPackageVersion,
    -- * UnitId helpers
    UnitId,
    Unit,
    unitString,
    stringToUnit,
#if !MIN_VERSION_ghc(9,0,0)
    pattern RealUnit,
#endif
    definiteUnitId,
    defUnitId,
    installedModule,
    -- * Module
    toUnitId,
    Development.IDE.GHC.Compat.Units.moduleUnitId,
    moduleUnit,
    -- * ExternalPackageState
    ExternalPackageState(..),
    -- * Utils
    filterInplaceUnits,
    FinderCache,
    showSDocForUser',
    ) where

import           Control.Monad
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as Map
#if MIN_VERSION_ghc(9,3,0)
import           GHC.Unit.Home.ModInfo
#endif
#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Data.ShortText              as ST
#if !MIN_VERSION_ghc(9,3,0)
import           GHC.Driver.Env                  (hsc_unit_dbs)
#endif
import           GHC.Driver.Ppr
import           GHC.Unit.Env
import           GHC.Unit.External
import           GHC.Unit.Finder
#else
import           GHC.Driver.Types
#endif
import           GHC.Data.FastString
import qualified GHC.Driver.Session              as DynFlags
import           GHC.Types.Unique.Set
import qualified GHC.Unit.Info                   as UnitInfo
import           GHC.Unit.State                  (LookupResult, UnitInfo,
                                                  UnitState (unitInfoMap))
import qualified GHC.Unit.State                  as State
import           GHC.Unit.Types                  hiding (moduleUnit, toUnitId)
import qualified GHC.Unit.Types                  as Unit
import           GHC.Utils.Outputable
#else
import qualified DynFlags
import           FastString
import           GhcPlugins                      (SDoc, showSDocForUser)
import           HscTypes
import           Module                          hiding (moduleUnitId)
import qualified Module
import           Packages                        (InstalledPackageInfo (haddockInterfaces, packageName),
                                                  LookupResult, PackageConfig,
                                                  PackageConfigMap,
                                                  PackageState,
                                                  getPackageConfigMap,
                                                  lookupPackage')
import qualified Packages
#endif

import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env
import           Development.IDE.GHC.Compat.Outputable
#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
import           Data.Map                        (Map)
#endif
import           Data.Either
import           Data.Version
import qualified GHC

#if MIN_VERSION_ghc(9,0,0)
type PreloadUnitClosure = UniqSet UnitId
#if MIN_VERSION_ghc(9,2,0)
type UnitInfoMap = State.UnitInfoMap
#else
type UnitInfoMap = Map UnitId UnitInfo
#endif
#else
type UnitState = PackageState
type UnitInfo = PackageConfig
type UnitInfoMap = PackageConfigMap
type PreloadUnitClosure = ()
type Unit = UnitId
#endif


#if !MIN_VERSION_ghc(9,0,0)
unitString :: Unit -> String
unitString = Module.unitIdString

stringToUnit :: String -> Unit
stringToUnit = Module.stringToUnitId
#endif

unitState :: HscEnv -> UnitState
#if MIN_VERSION_ghc(9,2,0)
unitState = ue_units . hsc_unit_env
#elif MIN_VERSION_ghc(9,0,0)
unitState = DynFlags.unitState . hsc_dflags
#else
unitState = DynFlags.pkgState . hsc_dflags
#endif

#if MIN_VERSION_ghc(9,3,0)
createUnitEnvFromFlags :: NE.NonEmpty DynFlags -> HomeUnitGraph
createUnitEnvFromFlags unitDflags =
  let
    newInternalUnitEnv dflags = mkHomeUnitEnv dflags emptyHomePackageTable Nothing
    unitEnvList = NE.map (\dflags -> (homeUnitId_ dflags, newInternalUnitEnv dflags)) unitDflags
  in
    unitEnv_new (Map.fromList (NE.toList (unitEnvList)))

initUnits :: [DynFlags] -> HscEnv -> IO HscEnv
initUnits unitDflags env = do
  let dflags0         = hsc_dflags env
  -- additionally, set checked dflags so we don't lose fixes
  let initial_home_graph = createUnitEnvFromFlags (dflags0 NE.:| unitDflags)
      home_units = unitEnv_keys initial_home_graph
  home_unit_graph <- forM initial_home_graph $ \homeUnitEnv -> do
    let cached_unit_dbs = homeUnitEnv_unit_dbs homeUnitEnv
        dflags = homeUnitEnv_dflags homeUnitEnv
        old_hpt = homeUnitEnv_hpt homeUnitEnv

    (dbs,unit_state,home_unit,mconstants) <- State.initUnits (hsc_logger env) dflags cached_unit_dbs home_units

    updated_dflags <- DynFlags.updatePlatformConstants dflags mconstants
    pure HomeUnitEnv
      { homeUnitEnv_units = unit_state
      , homeUnitEnv_unit_dbs = Just dbs
      , homeUnitEnv_dflags = updated_dflags
      , homeUnitEnv_hpt = old_hpt
      , homeUnitEnv_home_unit = Just home_unit
      }

  let dflags1 = homeUnitEnv_dflags $ unitEnv_lookup (homeUnitId_ dflags0) home_unit_graph
  let unit_env = UnitEnv
        { ue_platform        = targetPlatform dflags1
        , ue_namever         = GHC.ghcNameVersion dflags1
        , ue_home_unit_graph = home_unit_graph
        , ue_current_unit    = homeUnitId_ dflags0
        , ue_eps             = ue_eps (hsc_unit_env env)
        }
  pure $ hscSetFlags dflags1 $ hscSetUnitEnv unit_env env
#else
initUnits :: [DynFlags] -> HscEnv -> IO HscEnv
initUnits _df env = pure env -- Can't do anything here, oldInitUnits should already be called
#endif


-- | oldInitUnits only needs to modify DynFlags for GHC <9.2
-- For GHC >= 9.2, we need to set the hsc_unit_env also, that is
-- done later by initUnits
oldInitUnits :: DynFlags -> IO DynFlags
#if MIN_VERSION_ghc(9,2,0)
oldInitUnits = pure
#elif MIN_VERSION_ghc(9,0,0)
oldInitUnits dflags = do
  newFlags <- State.initUnits dflags
  pure newFlags
#else
oldInitUnits dflags = do
  newFlags <- fmap fst $ Packages.initPackages dflags
  pure newFlags
#endif

explicitUnits :: UnitState -> [Unit]
explicitUnits ue =
#if MIN_VERSION_ghc(9,3,0)
  map fst $ State.explicitUnits ue
#elif MIN_VERSION_ghc(9,0,0)
  State.explicitUnits ue
#else
  Packages.explicitPackages ue
#endif

listVisibleModuleNames :: HscEnv -> [ModuleName]
listVisibleModuleNames env =
#if MIN_VERSION_ghc(9,0,0)
  State.listVisibleModuleNames $ unitState env
#else
  Packages.listVisibleModuleNames $ hsc_dflags env
#endif

getUnitName :: HscEnv -> UnitId -> Maybe PackageName
getUnitName env i =
#if MIN_VERSION_ghc(9,0,0)
  State.unitPackageName <$> State.lookupUnitId (unitState env) i
#else
  packageName <$> Packages.lookupPackage (hsc_dflags env) (definiteUnitId (defUnitId i))
#endif

lookupModuleWithSuggestions
  :: HscEnv
  -> ModuleName
#if MIN_VERSION_ghc(9,3,0)
  -> GHC.PkgQual
#else
  -> Maybe FastString
#endif
  -> LookupResult
lookupModuleWithSuggestions env modname mpkg =
#if MIN_VERSION_ghc(9,0,0)
  State.lookupModuleWithSuggestions (unitState env) modname mpkg
#else
  Packages.lookupModuleWithSuggestions (hsc_dflags env) modname mpkg
#endif

getUnitInfoMap :: HscEnv -> UnitInfoMap
getUnitInfoMap =
#if MIN_VERSION_ghc(9,2,0)
  unitInfoMap . ue_units . hsc_unit_env
#elif MIN_VERSION_ghc(9,0,0)
  unitInfoMap . unitState
#else
  Packages.getPackageConfigMap . hsc_dflags
#endif

lookupUnit :: HscEnv -> Unit -> Maybe UnitInfo
#if MIN_VERSION_ghc(9,0,0)
lookupUnit env pid = State.lookupUnit (unitState env) pid
#else
lookupUnit env pid = Packages.lookupPackage (hsc_dflags env) pid
#endif

lookupUnit' :: Bool -> UnitInfoMap -> PreloadUnitClosure -> Unit -> Maybe UnitInfo
#if MIN_VERSION_ghc(9,0,0)
lookupUnit' = State.lookupUnit'
#else
lookupUnit' b pcm _ u = Packages.lookupPackage' b pcm u
#endif

preloadClosureUs :: HscEnv -> PreloadUnitClosure
#if MIN_VERSION_ghc(9,2,0)
preloadClosureUs = State.preloadClosure . unitState
#elif MIN_VERSION_ghc(9,0,0)
preloadClosureUs = State.preloadClosure . unitState
#else
preloadClosureUs = const ()
#endif

unitExposedModules :: UnitInfo -> [(ModuleName, Maybe Module)]
unitExposedModules ue =
#if MIN_VERSION_ghc(9,0,0)
  UnitInfo.unitExposedModules ue
#else
  Packages.exposedModules ue
#endif

unitDepends :: UnitInfo -> [UnitId]
#if MIN_VERSION_ghc(9,0,0)
unitDepends = State.unitDepends
#else
unitDepends = fmap (Module.DefiniteUnitId. defUnitId') . Packages.depends
#endif

unitPackageNameString :: UnitInfo -> String
unitPackageNameString =
#if MIN_VERSION_ghc(9,0,0)
  UnitInfo.unitPackageNameString
#else
  Packages.packageNameString
#endif

unitPackageVersion :: UnitInfo -> Version
unitPackageVersion =
#if MIN_VERSION_ghc(9,0,0)
  UnitInfo.unitPackageVersion
#else
  Packages.packageVersion
#endif

unitInfoId :: UnitInfo -> Unit
unitInfoId =
#if MIN_VERSION_ghc(9,0,0)
  UnitInfo.mkUnit
#else
  Packages.packageConfigId
#endif

unitHaddockInterfaces :: UnitInfo -> [FilePath]
unitHaddockInterfaces =
#if MIN_VERSION_ghc(9,2,0)
  fmap ST.unpack . UnitInfo.unitHaddockInterfaces
#elif MIN_VERSION_ghc(9,0,0)
  UnitInfo.unitHaddockInterfaces
#else
  haddockInterfaces
#endif

-- ------------------------------------------------------------------
-- Backwards Compatible UnitState
-- ------------------------------------------------------------------

-- ------------------------------------------------------------------
-- Patterns and helpful definitions
-- ------------------------------------------------------------------

#if MIN_VERSION_ghc(9,2,0)
definiteUnitId :: Definite uid -> GenUnit uid
definiteUnitId         = RealUnit
defUnitId :: unit -> Definite unit
defUnitId              = Definite
installedModule :: unit -> ModuleName -> GenModule unit
installedModule        = Module

#elif MIN_VERSION_ghc(9,0,0)
definiteUnitId         = RealUnit
defUnitId              = Definite
installedModule        = Module

#else
pattern RealUnit :: Module.DefUnitId -> UnitId
pattern RealUnit x = Module.DefiniteUnitId x

definiteUnitId :: Module.DefUnitId -> UnitId
definiteUnitId = Module.DefiniteUnitId

defUnitId :: UnitId -> Module.DefUnitId
defUnitId = Module.DefUnitId . Module.toInstalledUnitId

defUnitId' :: Module.InstalledUnitId -> Module.DefUnitId
defUnitId' = Module.DefUnitId

installedModule :: UnitId -> ModuleName -> Module.InstalledModule
installedModule uid modname = Module.InstalledModule (Module.toInstalledUnitId uid) modname
#endif

toUnitId :: Unit -> UnitId
toUnitId =
#if MIN_VERSION_ghc(9,0,0)
    Unit.toUnitId
#else
    id
#endif

moduleUnitId :: Module -> UnitId
moduleUnitId =
#if MIN_VERSION_ghc(9,0,0)
    Unit.toUnitId . Unit.moduleUnit
#else
    Module.moduleUnitId
#endif

moduleUnit :: Module -> Unit
moduleUnit =
#if MIN_VERSION_ghc(9,0,0)
    Unit.moduleUnit
#else
    Module.moduleUnitId
#endif

filterInplaceUnits :: [UnitId] -> [PackageFlag] -> ([UnitId], [PackageFlag])
filterInplaceUnits us packageFlags =
  partitionEithers (map isInplace packageFlags)
  where
    isInplace :: PackageFlag -> Either UnitId PackageFlag
    isInplace p@(ExposePackage _ (UnitIdArg u) _) =
#if MIN_VERSION_ghc(9,0,0)
      if toUnitId u `elem` us
        then Left $ toUnitId  u
        else Right p
#else
      if u `elem` us
        then Left u
        else Right p
#endif
    isInplace p = Right p

showSDocForUser' :: HscEnv -> PrintUnqualified -> SDoc -> String
#if MIN_VERSION_ghc(9,2,0)
showSDocForUser' env = showSDocForUser (hsc_dflags env) (unitState env)
#else
showSDocForUser' env = showSDocForUser (hsc_dflags env)
#endif
