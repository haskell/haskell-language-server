{-# LANGUAGE CPP #-}

-- | Plugin Compat utils.
module Development.IDE.GHC.Compat.Plugins (
    Plugin(..),
    defaultPlugin,
#if __GLASGOW_HASKELL__ >= 808
    PluginWithArgs(..),
#endif
    applyPluginsParsedResultAction,
    initializePlugins,

    -- * Static plugins
#if MIN_VERSION_ghc(8,8,0)
    StaticPlugin(..),
    hsc_static_plugins,
#endif
    ) where

#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Env                    as Env
#endif
import           GHC.Driver.Plugins                (Plugin (..),
                                                    PluginWithArgs (..),
                                                    StaticPlugin (..),
#if MIN_VERSION_ghc(9,3,0)
                                                    staticPlugins,
                                                    ParsedResult(..),
                                                    PsMessages(..),
#endif
                                                    defaultPlugin, withPlugins)
import qualified GHC.Runtime.Loader                as Loader
#elif MIN_VERSION_ghc(8,8,0)
import qualified DynamicLoading                    as Loader
import           Plugins
#else
import qualified DynamicLoading                    as Loader
import           Plugins                           (Plugin (..), defaultPlugin,
                                                    withPlugins)
#endif
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env    (hscSetFlags, hsc_dflags)
import           Development.IDE.GHC.Compat.Parser as Parser

applyPluginsParsedResultAction :: HscEnv -> DynFlags -> ModSummary -> Parser.ApiAnns -> ParsedSource -> IO ParsedSource
applyPluginsParsedResultAction env dflags ms hpm_annotations parsed = do
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
#if MIN_VERSION_ghc(9,3,0)
  fmap (hpm_module . parsedResultModule) $
#else
  fmap hpm_module $
#endif
    runHsc env $ withPlugins
#if MIN_VERSION_ghc(9,3,0)
      (Env.hsc_plugins env)
#elif MIN_VERSION_ghc(9,2,0)
      env
#else
      dflags
#endif
      applyPluginAction
#if MIN_VERSION_ghc(9,3,0)
      (ParsedResult (HsParsedModule parsed [] hpm_annotations) (PsMessages mempty mempty))
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


#if MIN_VERSION_ghc(8,8,0)
hsc_static_plugins :: HscEnv -> [StaticPlugin]
#if MIN_VERSION_ghc(9,3,0)
hsc_static_plugins = staticPlugins . Env.hsc_plugins
#elif MIN_VERSION_ghc(9,2,0)
hsc_static_plugins = Env.hsc_static_plugins
#else
hsc_static_plugins = staticPlugins . hsc_dflags
#endif
#endif
