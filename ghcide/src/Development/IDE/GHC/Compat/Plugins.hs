{-# LANGUAGE CPP #-}

-- | Plugin Compat utils.
module Development.IDE.GHC.Compat.Plugins (
    Plugin(..),
    defaultPlugin,
    PluginWithArgs(..),
    applyPluginsParsedResultAction,
    initializePlugins,
    initPlugins,

    -- * Static plugins
    StaticPlugin(..),
    hsc_static_plugins,
    ) where

#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Env                    as Env
#endif
import           GHC.Driver.Plugins                (Plugin (..),
                                                    PluginWithArgs (..),
                                                    StaticPlugin (..),
                                                    defaultPlugin, withPlugins)
#if MIN_VERSION_ghc(9,3,0)
import           GHC.Driver.Plugins                (ParsedResult (..),
                                                    PsMessages (..),
                                                    staticPlugins)
#endif
import qualified GHC.Runtime.Loader                as Loader
#else
import qualified DynamicLoading                    as Loader
import           Plugins
#endif
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env    (hscSetFlags, hsc_dflags)
import           Development.IDE.GHC.Compat.Parser as Parser
import Debug.Trace
import GHC.Driver.Env (hsc_plugins)
import GHC.Driver.Plugins

applyPluginsParsedResultAction :: HscEnv -> DynFlags -> ModSummary -> Parser.ApiAnns -> ParsedSource -> PsMessages -> IO (ParsedSource, PsMessages)
applyPluginsParsedResultAction env dflags ms hpm_annotations parsed msgs = do
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
#if MIN_VERSION_ghc(9,3,0)
  fmap (\result -> (hpm_module (parsedResultModule result), (parsedResultMessages result))) $ runHsc env $ withPlugins
#else
  fmap ((, msgs), hpm_module) $ runHsc env $ withPlugins
#endif
#if MIN_VERSION_ghc(9,3,0)
      (Env.hsc_plugins env)
#elif MIN_VERSION_ghc(9,2,0)
      env
#else
      dflags
#endif
      applyPluginAction
#if MIN_VERSION_ghc(9,3,0)
      (ParsedResult (HsParsedModule parsed [] hpm_annotations) msgs)
#else
      (HsParsedModule parsed [] hpm_annotations)
#endif

initializePlugins :: HscEnv -> IO HscEnv
initializePlugins env = do
#if MIN_VERSION_ghc(9,2,0)
    Loader.initializePlugins env
#else
    newDf <- Loader.initializePlugins env (hsc_dflags env)
    pure $ hscSetFlags newDf env
#endif

-- Plugins aren't stored in ModSummary anymore since GHC 9.0, but this
-- function still returns it for compatibility with 8.10
initPlugins :: HscEnv -> ModSummary -> IO (ModSummary, HscEnv)
initPlugins session modSummary = do
    session1 <- initializePlugins (hscSetFlags (ms_hspp_opts modSummary) session)
    return (modSummary{ms_hspp_opts = hsc_dflags session1}, session1)

hsc_static_plugins :: HscEnv -> [StaticPlugin]
#if MIN_VERSION_ghc(9,3,0)
hsc_static_plugins = staticPlugins . Env.hsc_plugins
#elif MIN_VERSION_ghc(9,2,0)
hsc_static_plugins = Env.hsc_static_plugins
#else
hsc_static_plugins = staticPlugins . hsc_dflags
#endif
