module Ide.Plugin.TypeSynonyms
  ( descriptor
  ) where

import Ide.Types
import Development.IDE

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
