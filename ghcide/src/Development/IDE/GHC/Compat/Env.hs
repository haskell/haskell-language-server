{-# LANGUAGE CPP #-}

-- | Compat module for the main Driver types, such as 'HscEnv',
-- 'UnitEnv' and some DynFlags compat functions.
module Development.IDE.GHC.Compat.Env (
    Env.HscEnv(hsc_FC, hsc_NC, hsc_IC, hsc_mod_graph, hsc_HPT, hsc_type_env_var),
    InteractiveContext(..),
    Env.hsc_dflags,
    hsc_logger,
    hsc_tmpfs,
    hsc_unit_env,
    hsc_hooks,
    hscSetHooks,
    TmpFs,
    -- * HomeUnit
    hscHomeUnit,
    HomeUnit,
    setHomeUnitId_,
    mkHomeModule,
    -- * Export so other compats works better
    Logger(..),
    UnitEnv,
    hscSetFlags,
    initTempFs,
    -- * Home Unit
    homeUnitId_,
    -- * DynFlags Helper
    setBytecodeLinkerOptions,
    Backend,
    setBackend,
    platformDefaultBackend,
    ) where

import GHC
#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Env as Env
import GHC.Unit.Env (UnitEnv)
import GHC.Utils.TmpFs
#else
import           GHC.Driver.Types (InteractiveContext(..))
import qualified GHC.Driver.Types as Env
#endif
import GHC.Driver.Hooks (Hooks)
import GHC.Unit.Types (UnitId, Unit)
import qualified GHC.Driver.Session as Home
import GHC.Driver.Session hiding (mkHomeModule)
#else
import HscTypes as Env
import Module (toInstalledUnitId)
import DynFlags (emptyFilesToClean, thisPackage, LogAction)
#if !MIN_VERSION_ghc(8,10,0)
import qualified DynFlags
#endif
import Hooks
#endif

import Data.IORef


#if !MIN_VERSION_ghc(9,2,0)
type UnitEnv = ()
newtype Logger = Logger { log_action :: LogAction }
type TmpFs = ()
#endif

setHomeUnitId_ :: UnitId -> DynFlags -> DynFlags
#if MIN_VERSION_ghc(9,2,0)
setHomeUnitId_ uid df = df { homeUnitId_ = uid }
#elif MIN_VERSION_ghc(9,0,0)
setHomeUnitId_ uid df = df { homeUnitId = uid }
#else
setHomeUnitId_ uid df = df { thisInstalledUnitId = toInstalledUnitId uid }
#endif

hscSetFlags :: DynFlags -> HscEnv -> HscEnv
hscSetFlags df env =
#if MIN_VERSION_ghc(9,2,0)
  hscSetFlags df env
#else
  env { Env.hsc_dflags = df }
#endif

initTempFs :: HscEnv -> IO HscEnv
initTempFs env = do
#if MIN_VERSION_ghc(9,2,0)
  tmpFs <- initTmpFs
  pure env { Env.hsc_tmpfs = tmpFs }
#else
  filesToClean <- newIORef emptyFilesToClean
  dirsToClean <- newIORef mempty
  let dflags = (Env.hsc_dflags env){filesToClean=filesToClean, dirsToClean=dirsToClean, useUnicode=True}
  pure $ hscSetFlags dflags env
#endif

hsc_unit_env :: HscEnv -> UnitEnv
hsc_unit_env =
#if MIN_VERSION_ghc(9,2,0)
  Env.hsc_unit_env
#else
  const ()
#endif

hsc_tmpfs :: HscEnv -> TmpFs
hsc_tmpfs =
#if MIN_VERSION_ghc(9,2,0)
  Env.hsc_tmpfs
#else
  const ()
#endif

hsc_logger :: HscEnv -> Logger
hsc_logger =
#if MIN_VERSION_ghc(9,2,0)
  Env.hsc_logger
#else
  Logger . GHC.log_action . Env.hsc_dflags
#endif

hsc_hooks :: HscEnv -> Hooks
hsc_hooks =
#if MIN_VERSION_ghc(9,2,0)
  Env.hsc_hooks
#else
  hooks . Env.hsc_dflags
#endif

hscSetHooks :: Hooks -> HscEnv -> HscEnv
hscSetHooks hooks env =
#if MIN_VERSION_ghc(9,2,0)
  env { Env.hsc_hooks = hooks }
#else
  hscSetFlags ((Env.hsc_dflags env) { hooks = hooks}) env
#endif

homeUnitId_ :: DynFlags -> UnitId
homeUnitId_ =
#if MIN_VERSION_ghc(9,2,0)
  homeUnitId_
#elif MIN_VERSION_ghc(9,0,0)
  homeUnitId
#else
  thisPackage
#endif


#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
type HomeUnit = Unit
#elif !MIN_VERSION_ghc(9,0,0)
type HomeUnit = UnitId
#endif

hscHomeUnit :: HscEnv -> HomeUnit
hscHomeUnit =
#if MIN_VERSION_ghc(9,2,0)
  ue_home_unit . Env.hsc_unit_env
#elif MIN_VERSION_ghc(9,0,0)
  homeUnit . Env.hsc_dflags
#else
  homeUnitId_ . hsc_dflags
#endif

mkHomeModule :: HomeUnit -> ModuleName -> Module
mkHomeModule =
#if MIN_VERSION_ghc(9,2,0)
  Home.mkHomeModule
#else
  mkModule
#endif

-- | We don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setBytecodeLinkerOptions :: DynFlags -> DynFlags
setBytecodeLinkerOptions df = df {
    ghcLink   = LinkInMemory
#if MIN_VERSION_ghc(9,2,0)
  , backend = NoBackend
#else
  , hscTarget = HscNothing
#endif
  , ghcMode = CompManager
    }


#if !MIN_VERSION_ghc(9,2,0)
type Backend = HscTarget
#endif

platformDefaultBackend :: DynFlags -> Backend
platformDefaultBackend =
#if MIN_VERSION_ghc(9,2,0)
  platformDefaultBackend . targetPlatform
#elif MIN_VERSION_ghc(8,10,0)
  defaultObjectTarget
#else
  defaultObjectTarget . DynFlags.targetPlatform
#endif

setBackend :: Backend -> DynFlags -> DynFlags
setBackend backend flags =
#if MIN_VERSION_ghc(9,2,0)
  flags { backend = backend }
#else
  flags { hscTarget = backend }
#endif

