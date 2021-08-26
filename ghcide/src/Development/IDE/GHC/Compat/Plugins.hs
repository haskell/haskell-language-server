{-# LANGUAGE CPP #-}

-- | Plugin Compat utils.
module Development.IDE.GHC.Compat.Plugins (
    applyPluginsParsedResultAction,
    initializePlugins,
    ) where

import GHC
#if MIN_VERSION_ghc(9,0,0)
#if MIN_VERSION_ghc(9,2,0)
import GHC.Driver.Env
#else
import GHC.Driver.Types
#endif
import GHC.Driver.Plugins (Plugin (parsedResultAction), withPlugins)
import qualified GHC.Runtime.Loader as Loader
import GHC.Parser.Lexer
#else
import Plugins (Plugin(parsedResultAction), withPlugins )
import qualified DynamicLoading as Loader
#endif
import Development.IDE.GHC.Compat.Core
import Development.IDE.GHC.Compat.Env (hscSetFlags, hsc_dflags)
import Development.IDE.GHC.Compat.Parser as Parser

applyPluginsParsedResultAction :: HscEnv -> DynFlags -> ModSummary -> Parser.ApiAnns -> ParsedSource -> IO (ParsedSource)
applyPluginsParsedResultAction env dflags ms hpm_annotations parsed = do
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
  fmap hpm_module $
    runHsc env $ withPlugins
#if MIN_VERSION_ghc(9,2,0)
      env
#else
      dflags
#endif
      applyPluginAction
      (mkHsParsedModule parsed [] hpm_annotations)

initializePlugins :: HscEnv -> IO HscEnv
initializePlugins env = do
#if MIN_VERSION_ghc(9,2,0)
    Loader.initializePlugins env
#else
    newDf <- Loader.initializePlugins env (hsc_dflags env)
    pure $ hscSetFlags newDf env
#endif
