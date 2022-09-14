module Ide.Plugin.Ecta (descriptor) where

import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import qualified Ecta.Plugin                     as Ecta
import           Ide.Types

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = (defaultPluginDescriptor pluginId)
  { pluginModifyDynflags = staticPlugin
  }

staticPlugin :: DynFlagsModifications
staticPlugin = mempty
  { dynFlagsModifyGlobal = \df -> df
    { staticPlugins = staticPlugins df <> [metaprogrammingPlugin]
    }
  }

ectaPlugin :: StaticPlugin
ectaPlugin = StaticPlugin $ PluginWithArgs Ecta.plugin []
