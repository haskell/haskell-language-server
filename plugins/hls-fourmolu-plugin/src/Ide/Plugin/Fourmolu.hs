module Ide.Plugin.Fourmolu (
    descriptor,
    LogEvent,
) where

import           Development.IDE            hiding (pluginHandlers)
import           Ide.Plugin.Fourmolu.Common
import           Ide.Plugin.Fourmolu.Impl
import           Ide.Types

descriptor :: Recorder (WithPriority LogEvent) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId pluginDesc)
        { pluginHandlers = handlers recorder plId
        , pluginConfigDescriptor = defaultConfigDescriptor{configCustomConfig = mkCustomConfig properties}
        }

