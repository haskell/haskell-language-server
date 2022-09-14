module Ide.Plugin.Ecta (descriptor) where

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import qualified ECTA.Plugin                     as Ecta
import           Ide.Types

descriptor :: PluginId -> PluginDescriptor s
descriptor pluginId = (defaultPluginDescriptor pluginId)
  { pluginModifyDynflags = staticPlugin
  }

staticPlugin :: DynFlagsModifications
staticPlugin = mempty
  { dynFlagsModifyGlobal = \df -> df
    { staticPlugins = staticPlugins df <> [ectaPlugin]
    }
  }

ectaPlugin :: StaticPlugin
ectaPlugin = StaticPlugin $ PluginWithArgs Ecta.plugin []
