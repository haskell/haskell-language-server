{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Compat module for 'UnitState' and 'UnitInfo'.
module Development.IDE.GHC.Compat.Units (
    -- * UnitState
    UnitState,
    initUnits,
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
    moduleUnitId,
    moduleUnit,
    -- * ExternalPackageState
    ExternalPackageState(..),
    -- * Utils
    filterInplaceUnits,
    FinderCache,
    ) where

#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Data.ShortText              as ST
import           GHC.Driver.Env                  (hsc_unit_dbs)
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
#else
import qualified DynFlags
import           FastString
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
#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
import           Data.Map                        (Map)
#endif
import           Data.Either
import           Data.Version

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

initUnits :: HscEnv -> IO HscEnv
initUnits env = do
#if MIN_VERSION_ghc(9,2,0)
  let dflags1         = hsc_dflags env
  -- Copied from GHC.setSessionDynFlags
  let cached_unit_dbs = hsc_unit_dbs env
  (dbs,unit_state,home_unit,mconstants) <- State.initUnits (hsc_logger env) dflags1 cached_unit_dbs

  dflags <- updatePlatformConstants dflags1 mconstants


  let unit_env = UnitEnv
        { ue_platform  = targetPlatform dflags
        , ue_namever   = ghcNameVersion dflags
        , ue_home_unit = home_unit
        , ue_units     = unit_state
        }
  pure $ hscSetFlags dflags $ hscSetUnitEnv unit_env env
    { hsc_unit_dbs = Just dbs
    }
#elif MIN_VERSION_ghc(9,0,0)
  newFlags <- State.initUnits $ hsc_dflags env
  pure $ hscSetFlags newFlags env
#else
  newFlags <- fmap fst . Packages.initPackages $ hsc_dflags env
  pure $ hscSetFlags newFlags env
#endif

explicitUnits :: UnitState -> [Unit]
explicitUnits ue =
#if MIN_VERSION_ghc(9,0,0)
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

lookupModuleWithSuggestions :: HscEnv -> ModuleName -> Maybe FastString -> LookupResult
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
definiteUnitId         = RealUnit
defUnitId              = Definite
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
