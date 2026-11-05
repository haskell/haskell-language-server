{-# LANGUAGE CPP #-}
module Development.IDE.GHC.Compat.Index
    ( UnitIndexCache
    , initUnitIndexCache
    , hscUIC
    , setHscUIC
    )
    where

#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 20260916)
import GHC.Unit.External.Index (UnitIndexCache, initUnitIndexCache)
import GHC.Driver.Env (hscUIC, HscEnv(hsc_unit_env))
import Development.IDE.GHC.Compat.Core (UnitEnv(ue_uic))
#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 20260916)
data UnitIndexCache = UnitIndexCache

hscUIC :: HscEnv -> IO UnitIndexCache
hscUIC _hsc_env = pure UnitIndexCache
#endif

setHscUIC :: UnitIndexCache -> HscEnv -> HscEnv
setHscUIC _uic hsc_env =
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 20260916)
    hsc_env { hsc_unit_env = (hsc_unit_env hsc_env) { ue_uic = _uic } }
#else
    hsc_env
#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 14, 1, 20260916)
initUnitIndexCache :: IO UnitIndexCache
initUnitIndexCache = pure UnitIndexCache
#endif
