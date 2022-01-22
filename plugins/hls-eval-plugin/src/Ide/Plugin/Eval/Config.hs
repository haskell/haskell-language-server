{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Eval.Config
  ( properties
  , getDiffProperty
  ) where

import           Ide.Plugin.Config     (Config)
import           Ide.Plugin.Properties
import           Ide.PluginUtils       (usePropertyLsp)
import           Ide.Types             (PluginId)
import           Language.LSP.Server   (MonadLsp)

properties :: Properties '[ 'PropertyKey "diff" 'TBoolean]
properties = emptyProperties
  & defineBooleanProperty #diff
    "Enable the diff output (WAS/NOW) of eval lenses" True

getDiffProperty :: (MonadLsp Config m) => PluginId -> m Bool
getDiffProperty plId = usePropertyLsp #diff plId properties
