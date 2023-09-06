{-# LANGUAGE CPP #-}

-- | Compat module for the main Driver types, such as 'HscEnv',
-- 'UnitEnv' and some DynFlags compat functions.
module Development.IDE.GHC.Compat.Env (
    Env.HscEnv(hsc_FC, hsc_NC, hsc_IC, hsc_mod_graph
#if MIN_VERSION_ghc(9,3,0)
              , hsc_type_env_vars
#else
              , hsc_type_env_var
#endif
              ),
    Env.hsc_HPT,
    InteractiveContext(..),
    setInteractivePrintName,
    setInteractiveDynFlags,
    Env.hsc_dflags,
    hsc_EPS,
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
    Development.IDE.GHC.Compat.Env.mkHomeModule,
    -- * Provide backwards Compatible
    -- types and helper functions.
    Logger(..),
    UnitEnv,
    hscSetUnitEnv,
    hscSetFlags,
    initTempFs,
    -- * Home Unit
    Development.IDE.GHC.Compat.Env.homeUnitId_,
    -- * DynFlags Helper
    setBytecodeLinkerOptions,
    setInterpreterLinkerOptions,
    Development.IDE.GHC.Compat.Env.safeImportsOn,
    -- * Ways
    Ways,
    Way,
    hostFullWays,
    setWays,
    wayGeneralFlags,
    wayUnsetGeneralFlags,
    -- * Backend, backwards compatible
    Backend,
    setBackend,
    Development.IDE.GHC.Compat.Env.platformDefaultBackend,
    ) where

import           GHC                                 (setInteractiveDynFlags)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import           GHC.Driver.Hooks                    (Hooks)
import           GHC.Driver.Session                  hiding (mkHomeModule)
import           GHC.Unit.Types                      (Module, UnitId)

#if !MIN_VERSION_ghc(9,2,0)
import qualified Data.Set                            as Set
import qualified GHC.Driver.Session                  as DynFlags
import           GHC.Driver.Types                    (HscEnv,
                                                      InteractiveContext (..),
                                                      hsc_EPS,
                                                      setInteractivePrintName)
import qualified GHC.Driver.Types                    as Env
import           GHC.Driver.Ways
import           GHC.Unit.Types                      (Unit, mkModule)
#endif

#if !MIN_VERSION_ghc(9,5,0)
import           GHC.Unit.Module.Name
#endif

#if !MIN_VERSION_ghc(9,2,0)
import           Data.IORef
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Backend                  as Backend
import qualified GHC.Driver.Env                      as Env
import qualified GHC.Driver.Session                  as Session
import           GHC.Platform.Ways
import           GHC.Runtime.Context
import           GHC.Unit.Env                        (UnitEnv)
import           GHC.Unit.Home                       as Home
import           GHC.Utils.Logger
import           GHC.Utils.TmpFs
#endif

#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,3,0)
import           GHC.Driver.Env                      (HscEnv, hsc_EPS)
#endif

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Driver.Env                      (HscEnv)
#endif

#if MIN_VERSION_ghc(9,5,0)
import           Language.Haskell.Syntax.Module.Name
#endif

#if MIN_VERSION_ghc(9,3,0)
hsc_EPS :: HscEnv -> UnitEnv
hsc_EPS = hsc_unit_env
#endif

#if !MIN_VERSION_ghc(9,2,0)
type UnitEnv = ()
newtype Logger = Logger { log_action :: LogAction }
type TmpFs = ()
#endif

setHomeUnitId_ :: UnitId -> DynFlags -> DynFlags
#if MIN_VERSION_ghc(9,2,0)
setHomeUnitId_ uid df = df { Session.homeUnitId_ = uid }
#else
setHomeUnitId_ uid df = df { homeUnitId = uid }
#endif

hscSetFlags :: DynFlags -> HscEnv -> HscEnv
hscSetFlags df env = env { Env.hsc_dflags = df }

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

hscSetUnitEnv :: UnitEnv -> HscEnv -> HscEnv
#if MIN_VERSION_ghc(9,2,0)
hscSetUnitEnv ue env = env { Env.hsc_unit_env = ue }
#else
hscSetUnitEnv _ env  = env
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
  Logger . DynFlags.log_action . Env.hsc_dflags
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
  Session.homeUnitId_
#else
  homeUnitId
#endif

safeImportsOn :: DynFlags -> Bool
safeImportsOn =
#if MIN_VERSION_ghc(9,2,0)
  Session.safeImportsOn
#else
  DynFlags.safeImportsOn
#endif

#if !MIN_VERSION_ghc(9,2,0)
type HomeUnit = Unit
#endif

hscHomeUnit :: HscEnv -> HomeUnit
hscHomeUnit =
#if MIN_VERSION_ghc(9,2,0)
  Env.hsc_home_unit
#else
  homeUnit . Env.hsc_dflags
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
#if MIN_VERSION_ghc(9,5,0)
  , backend = noBackend
#elif MIN_VERSION_ghc(9,2,0)
  , backend = NoBackend
#else
  , hscTarget = HscNothing
#endif
  , ghcMode = CompManager
    }

setInterpreterLinkerOptions :: DynFlags -> DynFlags
setInterpreterLinkerOptions df = df {
    ghcLink   = LinkInMemory
#if MIN_VERSION_ghc(9,5,0)
   , backend = interpreterBackend
#elif MIN_VERSION_ghc(9,2,0)
  , backend = Interpreter
#else
  , hscTarget = HscInterpreted
#endif
  , ghcMode = CompManager
    }

-- -------------------------------------------------------
-- Ways helpers
-- -------------------------------------------------------

#if !MIN_VERSION_ghc(9,2,0)
type Ways = Set.Set Way
#endif

setWays :: Ways -> DynFlags -> DynFlags
setWays newWays flags =
#if MIN_VERSION_ghc(9,2,0)
  flags { Session.targetWays_ = newWays}
#else
  flags {ways = newWays}
#endif

-- -------------------------------------------------------
-- Backend helpers
-- -------------------------------------------------------

#if !MIN_VERSION_ghc(9,2,0)
type Backend = HscTarget
#endif

platformDefaultBackend :: DynFlags -> Backend
platformDefaultBackend =
#if MIN_VERSION_ghc(9,2,0)
  Backend.platformDefaultBackend . targetPlatform
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

