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
import           GHC.Driver.Plugins                (ParsedResult (..),
                                                    Plugin (..),
                                                    PluginWithArgs (..),
                                                    PsMessages (..),
                                                    StaticPlugin (..),
                                                    defaultPlugin,
                                                    staticPlugins, withPlugins)
import qualified GHC.Parser.Lexer                  as Lexer
import qualified GHC.Runtime.Loader                as Loader


getPsMessages :: PState -> PsMessages
getPsMessages pst =
  uncurry PsMessages $ Lexer.getPsMessages pst

applyPluginsParsedResultAction :: HscEnv -> ModSummary -> ParsedSource -> PsMessages -> IO (ParsedSource, PsMessages)
applyPluginsParsedResultAction env ms parsed msgs = do
  -- Apply parsedResultAction of plugins
  let applyPluginAction p opts = parsedResultAction p opts ms
  fmap (\result -> (hpm_module (parsedResultModule result), parsedResultMessages result)) $ runHsc env $ withPlugins
      (Env.hsc_plugins env)
      applyPluginAction
      (ParsedResult (HsParsedModule parsed []) msgs)

initializePlugins :: HscEnv -> IO HscEnv
initializePlugins env = do
    Loader.initializePlugins env

-- | Plugins aren't stored in ModSummary anymore since GHC 9.2, so this
-- function not returns ModSummary
initPlugins :: HscEnv -> ModSummary -> IO HscEnv
initPlugins session modSummary = do
    session1 <- initializePlugins (hscSetFlags (ms_hspp_opts modSummary) session)
    return session1

hsc_static_plugins :: HscEnv -> [StaticPlugin]
hsc_static_plugins = staticPlugins . Env.hsc_plugins
