{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wwarn #-}

{- |
Eval Plugin entry point.
-}
module Ide.Plugin.Eval (
    descriptor,
) where

import Development.IDE (IdeState)
import qualified Ide.Plugin.Eval.CodeLens as CL
import Ide.Types (
    PluginDescriptor (..),
    PluginId,
    defaultPluginDescriptor,
 )

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginCodeLensProvider = Just CL.codeLens
        , pluginCommands = [CL.evalCommand]
        }
