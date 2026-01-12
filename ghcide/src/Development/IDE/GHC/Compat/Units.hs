{-# LANGUAGE CPP #-}

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
    mkUnit,
    unitPackageNameString,
    unitPackageVersion,
    -- * UnitId helpers
    UnitId,
    Unit,
    unitString,
    stringToUnit,
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
    showSDocForUser',
    findImportedModule,
    ) where

import           Data.Either
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env
import           Development.IDE.GHC.Compat.Outputable
import           Prelude                               hiding (mod)

import           Control.Monad
import qualified Data.List.NonEmpty                    as NE
import qualified Data.Map.Strict                       as Map
import qualified GHC
import qualified GHC.Data.ShortText                    as ST
import qualified GHC.Driver.Session                    as DynFlags
import           GHC.Types.PkgQual                     (PkgQual (NoPkgQual))
import           GHC.Types.Unique.Set
import           GHC.Unit.External
import qualified GHC.Unit.Finder                       as GHC
import           GHC.Unit.Home.ModInfo
import qualified GHC.Unit.Info                         as UnitInfo
import           GHC.Unit.State                        (LookupResult, UnitInfo,
                                                        UnitInfoMap,
                                                        UnitState (unitInfoMap),
                                                        lookupUnit', mkUnit,
                                                        unitDepends,
                                                        unitExposedModules,
                                                        unitPackageNameString,
                                                        unitPackageVersion)
import qualified GHC.Unit.State                        as State
import           GHC.Unit.Types

#if MIN_VERSION_ghc(9,13,0)
import qualified Data.Set                              as Set
import           GHC.Unit.Home.Graph
import           GHC.Unit.Home.PackageTable            (emptyHomePackageTable)
import           GHC.Unit.Module.Graph                 (emptyMG)
#endif


type PreloadUnitClosure = UniqSet UnitId

unitState :: HscEnv -> UnitState
unitState = ue_units . hsc_unit_env

createUnitEnvFromFlags :: NE.NonEmpty DynFlags -> IO HomeUnitGraph
createUnitEnvFromFlags unitDflags = do
#if MIN_VERSION_ghc(9,13,0)
  let mkEntry dflags = do
        hpt <- emptyHomePackageTable
        let us = State.emptyUnitState -- placeholder UnitState
        pure (homeUnitId_ dflags, mkHomeUnitEnv us Nothing dflags hpt Nothing)
  unitEnvList <- mapM mkEntry (NE.toList unitDflags)
  pure $ unitEnv_new (Map.fromList unitEnvList)
#else
  let newInternalUnitEnv dflags = mkHomeUnitEnv dflags emptyHomePackageTable Nothing
      unitEnvList = NE.map (\dflags -> (homeUnitId_ dflags, newInternalUnitEnv dflags)) unitDflags
  pure $ unitEnv_new (Map.fromList (NE.toList unitEnvList))
#endif

initUnits :: [DynFlags] -> HscEnv -> IO HscEnv
initUnits unitDflags env = do
  let dflags0         = hsc_dflags env
  -- additionally, set checked dflags so we don't lose fixes
  initial_home_graph <- createUnitEnvFromFlags (dflags0 NE.:| unitDflags)
  let home_units = unitEnv_keys initial_home_graph
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
#if MIN_VERSION_ghc(9,13,0)
        , ue_module_graph    = emptyMG
#endif
        }
  pure $ hscSetFlags dflags1 $ hscSetUnitEnv unit_env env


explicitUnits :: UnitState -> [Unit]
explicitUnits ue =
  map fst $ State.explicitUnits ue

listVisibleModuleNames :: HscEnv -> [ModuleName]
listVisibleModuleNames env =
  State.listVisibleModuleNames $ unitState env

getUnitName :: HscEnv -> UnitId -> Maybe PackageName
getUnitName env i =
  State.unitPackageName <$> State.lookupUnitId (unitState env) i

lookupModuleWithSuggestions
  :: HscEnv
  -> ModuleName
  -> GHC.PkgQual
  -> LookupResult
lookupModuleWithSuggestions env modname mpkg =
  State.lookupModuleWithSuggestions (unitState env) modname mpkg

getUnitInfoMap :: HscEnv -> UnitInfoMap
getUnitInfoMap =
  unitInfoMap . ue_units . hsc_unit_env

lookupUnit :: HscEnv -> Unit -> Maybe UnitInfo
lookupUnit env pid = State.lookupUnit (unitState env) pid

preloadClosureUs :: HscEnv -> PreloadUnitClosure
preloadClosureUs = State.preloadClosure . unitState

unitHaddockInterfaces :: UnitInfo -> [FilePath]
unitHaddockInterfaces =
  fmap ST.unpack . UnitInfo.unitHaddockInterfaces

-- ------------------------------------------------------------------
-- Backwards Compatible UnitState
-- ------------------------------------------------------------------

-- ------------------------------------------------------------------
-- Patterns and helpful definitions
-- ------------------------------------------------------------------

definiteUnitId :: Definite uid -> GenUnit uid
definiteUnitId         = RealUnit
defUnitId :: unit -> Definite unit
defUnitId              = Definite
installedModule :: unit -> ModuleName -> GenModule unit
installedModule        = Module


filterInplaceUnits :: [UnitId] -> [PackageFlag] -> ([UnitId], [PackageFlag])
filterInplaceUnits us packageFlags =
  partitionEithers (map isInplace packageFlags)
  where
    isInplace :: PackageFlag -> Either UnitId PackageFlag
    isInplace p@(ExposePackage _ (UnitIdArg u) _) =
      if toUnitId u `elem` us
        then Left $ toUnitId  u
        else Right p
    isInplace p = Right p

showSDocForUser' :: HscEnv -> PrintUnqualified -> SDoc -> String
showSDocForUser' env = showSDocForUser (hsc_dflags env) (unitState env)

findImportedModule :: HscEnv -> ModuleName -> IO (Maybe Module)
findImportedModule env mn = do
    res <- GHC.findImportedModule env mn NoPkgQual
    case res of
        Found _ mod -> pure . pure $ mod
        _           -> pure Nothing
