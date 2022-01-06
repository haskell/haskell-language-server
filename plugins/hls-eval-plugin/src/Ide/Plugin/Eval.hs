{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wwarn #-}

{- |
Eval Plugin entry point.
-}
module Ide.Plugin.Eval (
    descriptor,
    Log
) where

import           Development.IDE              (IdeState)
import           Development.IDE.Types.Logger (Recorder, cmap)
import qualified Ide.Plugin.Eval.CodeLens     as CL
import           Ide.Plugin.Eval.Rules        (rules)
import qualified Ide.Plugin.Eval.Rules        as EvalRules
import           Ide.Types                    (PluginDescriptor (..), PluginId,
                                               defaultPluginDescriptor,
                                               mkPluginHandler)
import           Language.LSP.Types

data Log = LogEvalRules EvalRules.Log deriving Show

-- |Plugin descriptor
descriptor :: Recorder Log -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkPluginHandler STextDocumentCodeLens CL.codeLens
        , pluginCommands = [CL.evalCommand]
        , pluginRules = rules (cmap LogEvalRules recorder)
        }
