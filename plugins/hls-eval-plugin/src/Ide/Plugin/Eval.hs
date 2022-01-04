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
import           Ide.Plugin.Eval.Rules    (rules)
import           Ide.Types                (PluginDescriptor (..), PluginId,
                                           defaultPluginDescriptor,
                                           mkPluginHandler)
import           Language.LSP.Types

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkPluginHandler STextDocumentCodeLens CL.codeLens
        , pluginCommands = [CL.evalCommand]
        , pluginRules = rules
        }
