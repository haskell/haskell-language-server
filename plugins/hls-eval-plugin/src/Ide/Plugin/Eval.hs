{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE LambdaCase            #-}

{- |
Eval Plugin entry point.
-}
module Ide.Plugin.Eval (
    descriptor,
    Log(..)
    ) where

import           Development.IDE               (IdeState)
import           Ide.Logger                    (Pretty (pretty), Recorder,
                                                WithPriority, cmapWithPrio)
import qualified Ide.Plugin.Eval.CodeLens      as CL
import           Ide.Plugin.Eval.Config
import           Ide.Plugin.Eval.Rules         (rules)
import qualified Ide.Plugin.Eval.Rules         as EvalRules
import           Ide.Types                     (ConfigDescriptor (..),
                                                PluginDescriptor (..), PluginId,
                                                defaultConfigDescriptor,
                                                defaultPluginDescriptor,
                                                mkCustomConfig, mkPluginHandler)
import           Language.LSP.Protocol.Message

newtype Log = LogEvalRules EvalRules.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogEvalRules log -> pretty log

-- |Plugin descriptor
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provies a code lens to evaluate expressions in doctest comments")
        { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeLens CL.codeLens
        , pluginCommands = [CL.evalCommand plId]
        , pluginRules = rules (cmapWithPrio LogEvalRules recorder)
        , pluginConfigDescriptor = defaultConfigDescriptor
                                   { configCustomConfig = mkCustomConfig properties
                                   }
        }
