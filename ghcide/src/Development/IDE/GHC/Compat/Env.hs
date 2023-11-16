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
    ghciBackend,
    Development.IDE.GHC.Compat.Env.platformDefaultBackend,
    ) where

import           GHC                                 (setInteractiveDynFlags)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import           GHC.Driver.Hooks                    (Hooks)
import           GHC.Driver.Session                  hiding (mkHomeModule)
import           GHC.Unit.Types                      (Module, UnitId)


#if !MIN_VERSION_ghc(9,5,0)
import           GHC.Unit.Module.Name
#endif


import           GHC.Driver.Backend                  as Backend
import qualified GHC.Driver.Env                      as Env
import qualified GHC.Driver.Session                  as Session
import           GHC.Platform.Ways
import           GHC.Runtime.Context
import           GHC.Unit.Env                        (UnitEnv)
import           GHC.Unit.Home                       as Home
import           GHC.Utils.Logger
import           GHC.Utils.TmpFs

#if !MIN_VERSION_ghc(9,3,0)
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

hsc_unit_env :: HscEnv -> UnitEnv
hsc_unit_env =
  Env.hsc_unit_env

hsc_tmpfs :: HscEnv -> TmpFs
hsc_tmpfs =
  Env.hsc_tmpfs

hsc_logger :: HscEnv -> Logger
hsc_logger =
  Env.hsc_logger

hsc_hooks :: HscEnv -> Hooks
hsc_hooks =
  Env.hsc_hooks

hscSetHooks :: Hooks -> HscEnv -> HscEnv
hscSetHooks hooks env =
  env { Env.hsc_hooks = hooks }

homeUnitId_ :: DynFlags -> UnitId
homeUnitId_ =
  Session.homeUnitId_

safeImportsOn :: DynFlags -> Bool
safeImportsOn =
  Session.safeImportsOn


hscHomeUnit :: HscEnv -> HomeUnit
hscHomeUnit =
  Env.hsc_home_unit

mkHomeModule :: HomeUnit -> ModuleName -> Module
mkHomeModule =
  Home.mkHomeModule

-- | We don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setBytecodeLinkerOptions :: DynFlags -> DynFlags
setBytecodeLinkerOptions df = df {
    ghcLink   = LinkInMemory
#if MIN_VERSION_ghc(9,5,0)
  , backend = noBackend
#else
  , backend = NoBackend
#endif
  , ghcMode = CompManager
    }

setInterpreterLinkerOptions :: DynFlags -> DynFlags
setInterpreterLinkerOptions df = df {
    ghcLink   = LinkInMemory
#if MIN_VERSION_ghc(9,5,0)
   , backend = interpreterBackend
#else
  , backend = Interpreter
#endif
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

