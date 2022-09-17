module Ide.Plugin.Hectare (descriptor) where

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import qualified ECTA.Plugin                     as GHC
import           Ide.Types

descriptor :: PluginId -> PluginDescriptor s
descriptor pluginId = (defaultPluginDescriptor pluginId)
  { pluginModifyDynflags = staticPlugin
  }

staticPlugin :: DynFlagsModifications
staticPlugin = mempty
  { dynFlagsModifyGlobal = \df -> df
    { staticPlugins = staticPlugins df <> [hectarePlugin]
    }
  }

hectarePlugin :: StaticPlugin
hectarePlugin = StaticPlugin $ PluginWithArgs GHC.plugin []
