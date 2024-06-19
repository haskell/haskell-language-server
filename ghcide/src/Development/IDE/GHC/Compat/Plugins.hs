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
import           Development.IDE.GHC.Compat.Env    (hscSetFlags, hsc_dflags)
import           Development.IDE.GHC.Compat.Parser as Parser

import qualified GHC.Driver.Env                    as Env
import           GHC.Driver.Plugins                (Plugin (..),
                                                    PluginWithArgs (..),
                                                    StaticPlugin (..),
                                                    defaultPlugin, withPlugins)
import qualified GHC.Runtime.Loader                as Loader

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]


import           GHC.Driver.Plugins                (ParsedResult (..),
                                                    PsMessages (..),
                                                    staticPlugins)
import qualified GHC.Parser.Lexer                  as Lexer


getPsMessages :: PState -> PsMessages
getPsMessages pst =
  uncurry PsMessages $ Lexer.getPsMessages pst

applyPluginsParsedResultAction :: HscEnv -> ModSummary -> Parser.ApiAnns -> ParsedSource -> PsMessages -> IO (ParsedSource, PsMessages)
applyPluginsParsedResultAction env ms hpm_annotations parsed msgs = do
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
  fmap (\result -> (hpm_module (parsedResultModule result), (parsedResultMessages result))) $ runHsc env $ withPlugins
      (Env.hsc_plugins env)
      applyPluginAction
      (ParsedResult (HsParsedModule parsed [] hpm_annotations) msgs)

initializePlugins :: HscEnv -> IO HscEnv
initializePlugins env = do
    Loader.initializePlugins env

-- | Plugins aren't stored in ModSummary anymore since GHC 9.2, but this
-- function still returns it for compatibility with 8.10
initPlugins :: HscEnv -> ModSummary -> IO (ModSummary, HscEnv)
initPlugins session modSummary = do
    session1 <- initializePlugins (hscSetFlags (ms_hspp_opts modSummary) session)
    return (modSummary{ms_hspp_opts = hsc_dflags session1}, session1)

hsc_static_plugins :: HscEnv -> [StaticPlugin]
hsc_static_plugins = staticPlugins . Env.hsc_plugins
