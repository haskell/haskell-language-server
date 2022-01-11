{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE LambdaCase            #-}

{- |
Eval Plugin entry point.
-}
module Ide.Plugin.Eval (
    descriptor,
    Log
, logToPriority) where

import           Development.IDE              (IdeState)
import           Development.IDE.Types.Logger (Recorder, cmap)
import qualified Development.IDE.Types.Logger as Logger
import qualified Ide.Plugin.Eval.CodeLens     as CL
import           Ide.Plugin.Eval.Rules        (rules)
import qualified Ide.Plugin.Eval.Rules        as EvalRules
import           Ide.Types                    (PluginDescriptor (..), PluginId,
                                               defaultPluginDescriptor,
                                               mkPluginHandler)
import           Language.LSP.Types
import           Prettyprinter                (Pretty (pretty))

newtype Log = LogEvalRules EvalRules.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogEvalRules evalRulesLog -> pretty evalRulesLog

logToPriority :: Log -> Logger.Priority
logToPriority = \case
  LogEvalRules log -> EvalRules.logToPriority log

-- |Plugin descriptor
descriptor :: Recorder Log -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkPluginHandler STextDocumentCodeLens CL.codeLens
        , pluginCommands = [CL.evalCommand]
        , pluginRules = rules (cmap LogEvalRules recorder)
        }
