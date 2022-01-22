{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wwarn #-}

{- |
Eval Plugin entry point.
-}
module Ide.Plugin.Eval (
    descriptor,
) where

import           Development.IDE          (IdeState)
import qualified Ide.Plugin.Eval.CodeLens as CL
import           Ide.Plugin.Eval.Config
import           Ide.Plugin.Eval.Rules    (rules)
import           Ide.Types                (ConfigDescriptor (..),
                                           PluginDescriptor (..), PluginId,
                                           defaultConfigDescriptor,
                                           defaultPluginDescriptor,
                                           mkCustomConfig, mkPluginHandler)
import           Language.LSP.Types

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkPluginHandler STextDocumentCodeLens CL.codeLens
        , pluginCommands = [CL.evalCommand plId]
        , pluginRules = rules
        , pluginConfigDescriptor = defaultConfigDescriptor
                                   { configCustomConfig = mkCustomConfig properties
                                   }
        }
