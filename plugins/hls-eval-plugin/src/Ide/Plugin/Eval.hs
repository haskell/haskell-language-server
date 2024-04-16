{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE LambdaCase            #-}

{- |
Eval Plugin entry point.
-}
module Ide.Plugin.Eval (
    descriptor,
    Eval.Log(..)
    ) where

import           Development.IDE               (IdeState)
import           Ide.Logger                    (Recorder, WithPriority)
import qualified Ide.Plugin.Eval.CodeLens      as CL
import           Ide.Plugin.Eval.Config
import           Ide.Plugin.Eval.Rules         (rules)
import qualified Ide.Plugin.Eval.Types         as Eval
import           Ide.Types                     (ConfigDescriptor (..),
                                                PluginDescriptor (..), PluginId,
                                                defaultConfigDescriptor,
                                                defaultPluginDescriptor,
                                                mkCustomConfig, mkPluginHandler)
import           Language.LSP.Protocol.Message

-- |Plugin descriptor
descriptor :: Recorder (WithPriority Eval.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provies a code lens to evaluate expressions in doctest comments")
        { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeLens (CL.codeLens recorder)
        , pluginCommands = [CL.evalCommand recorder plId]
        , pluginRules = rules recorder
        , pluginConfigDescriptor = defaultConfigDescriptor
                                   { configCustomConfig = mkCustomConfig properties
                                   }
        }
