{-# LANGUAGE CPP #-}

-- | Compat module for the main Driver types, such as 'HscEnv',
-- 'UnitEnv' and some DynFlags compat functions.
module Development.IDE.GHC.Compat.Env (
    Env.HscEnv(hsc_FC, hsc_NC, hsc_IC
              , hsc_type_env_vars
              ),
    Env.hsc_mod_graph,
    Env.hsc_HPT,
    InteractiveContext(..),
    setInteractivePrintName,
    setInteractiveDynFlags,
    Env.hsc_dflags,
    hsc_EPS,
    Env.hsc_logger,
    Env.hsc_tmpfs,
    Env.hsc_unit_env,
    Env.hsc_hooks,
    hscSetHooks,
    TmpFs,
    -- * HomeUnit
    hscHomeUnit,
    HomeUnit,
    setHomeUnitId_,
    Home.mkHomeModule,
    -- * Provide backwards Compatible
    -- types and helper functions.
    Logger,
    UnitEnv,
    hscSetUnitEnv,
    hscSetFlags,
    initTempFs,
    -- * Home Unit
    Session.homeUnitId_,
    -- * DynFlags Helper
    setBytecodeLinkerOptions,
    setInterpreterLinkerOptions,
    Session.safeImportsOn,
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
    ghciBackend,
    Development.IDE.GHC.Compat.Env.platformDefaultBackend,
    workingDirectory,
    setWorkingDirectory,
    hscSetActiveUnitId,
    reexportedModules,
    ) where

import           GHC                 (setInteractiveDynFlags)

import           GHC.Driver.Backend  as Backend
import           GHC.Driver.Env      (HscEnv, hscSetActiveUnitId)
import qualified GHC.Driver.Env      as Env
import           GHC.Driver.Hooks    (Hooks)
import           GHC.Driver.Session
import qualified GHC.Driver.Session  as Session
import           GHC.Platform.Ways
import           GHC.Runtime.Context
import           GHC.Unit.Env        (UnitEnv)
import           GHC.Unit.Home       as Home
import           GHC.Unit.Types      (UnitId)
import           GHC.Utils.Logger
import           GHC.Utils.TmpFs


hsc_EPS :: HscEnv -> UnitEnv
hsc_EPS = Env.hsc_unit_env

setWorkingDirectory :: FilePath -> DynFlags -> DynFlags
setWorkingDirectory p d = d { workingDirectory =  Just p }

setHomeUnitId_ :: UnitId -> DynFlags -> DynFlags
setHomeUnitId_ uid df = df { Session.homeUnitId_ = uid }

hscSetFlags :: DynFlags -> HscEnv -> HscEnv
hscSetFlags df env = env { Env.hsc_dflags = df }

initTempFs :: HscEnv -> IO HscEnv
initTempFs env = do
  tmpFs <- initTmpFs
  pure env { Env.hsc_tmpfs = tmpFs }

hscSetUnitEnv :: UnitEnv -> HscEnv -> HscEnv
hscSetUnitEnv ue env = env { Env.hsc_unit_env = ue }

hscSetHooks :: Hooks -> HscEnv -> HscEnv
hscSetHooks hooks env =
  env { Env.hsc_hooks = hooks }

hscHomeUnit :: HscEnv -> HomeUnit
hscHomeUnit =
  Env.hsc_home_unit

-- | We don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setBytecodeLinkerOptions :: DynFlags -> DynFlags
setBytecodeLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , backend = noBackend
  , ghcMode = CompManager
    }

setInterpreterLinkerOptions :: DynFlags -> DynFlags
setInterpreterLinkerOptions df = df {
    ghcLink   = LinkInMemory
   , backend = interpreterBackend
  , ghcMode = CompManager
    }

-- -------------------------------------------------------
-- Ways helpers
-- -------------------------------------------------------


setWays :: Ways -> DynFlags -> DynFlags
setWays newWays flags =
  flags { Session.targetWays_ = newWays}

-- -------------------------------------------------------
-- Backend helpers
-- -------------------------------------------------------


ghciBackend  :: Backend
#if MIN_VERSION_ghc(9,6,0)
ghciBackend = interpreterBackend
#else
ghciBackend = Interpreter
#endif

platformDefaultBackend :: DynFlags -> Backend
platformDefaultBackend =
  Backend.platformDefaultBackend . targetPlatform

setBackend :: Backend -> DynFlags -> DynFlags
setBackend backend flags =
  flags { backend = backend }

