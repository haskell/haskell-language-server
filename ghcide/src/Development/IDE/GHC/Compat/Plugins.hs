{-# LANGUAGE CPP #-}

-- | Plugin Compat utils.
module Development.IDE.GHC.Compat.Plugins (
    -- * Plugin Compat Types, and initialisation
    Plugin(..),
    defaultPlugin,
    PluginWithArgs(..),
    applyPluginsParsedResultAction,
    initializePlugins,
    initPlugins,

    -- * Static plugins
    StaticPlugin(..),
    hsc_static_plugins,

    -- * Plugin messages
    PsMessages(..),
    getPsMessages
    ) where

import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env        (hscSetFlags, hsc_dflags)
import           Development.IDE.GHC.Compat.Parser     as Parser

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import           GHC.Driver.Plugins                    (Plugin (..),
                                                        PluginWithArgs (..),
                                                        StaticPlugin (..),
                                                        defaultPlugin,
                                                        withPlugins)
import qualified GHC.Runtime.Loader                    as Loader

#if !MIN_VERSION_ghc(9,3,0)
import           Development.IDE.GHC.Compat.Outputable as Out
#endif

#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Driver.Env                        as Env
#endif

#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,3,0)
import           Data.Bifunctor                        (bimap)
#endif

#if !MIN_VERSION_ghc(9,3,0)
import           Development.IDE.GHC.Compat.Util       (Bag)
#endif

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Driver.Plugins                    (ParsedResult (..),
                                                        PsMessages (..),
                                                        staticPlugins)
import qualified GHC.Parser.Lexer                      as Lexer
#endif


#if !MIN_VERSION_ghc(9,3,0)
type PsMessages = (Bag WarnMsg, Bag ErrMsg)
#endif

getPsMessages :: PState -> DynFlags -> PsMessages
getPsMessages pst _dflags = --dfags is only used if GHC < 9.2
#if MIN_VERSION_ghc(9,3,0)
  uncurry PsMessages $ Lexer.getPsMessages pst
#else
#if MIN_VERSION_ghc(9,2,0)
                 bimap (fmap pprWarning) (fmap pprError) $
#endif
                 getMessages pst
#if !MIN_VERSION_ghc(9,2,0)
                   _dflags
#endif
#endif

applyPluginsParsedResultAction :: HscEnv -> DynFlags -> ModSummary -> Parser.ApiAnns -> ParsedSource -> PsMessages -> IO (ParsedSource, PsMessages)
applyPluginsParsedResultAction env _dflags ms hpm_annotations parsed msgs = do
  -- dflags is only used in GHC < 9.2
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
#if MIN_VERSION_ghc(9,3,0)
  fmap (\result -> (hpm_module (parsedResultModule result), (parsedResultMessages result))) $ runHsc env $ withPlugins
#else
  fmap (\parsed_module -> (hpm_module parsed_module, msgs)) $ runHsc env $ withPlugins
#endif
#if MIN_VERSION_ghc(9,3,0)
      (Env.hsc_plugins env)
#elif MIN_VERSION_ghc(9,2,0)
      env
#else
      _dflags
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

-- | Plugins aren't stored in ModSummary anymore since GHC 9.2, but this
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
