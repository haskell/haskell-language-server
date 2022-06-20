module Ide.Plugin.Class (descriptor, Log(..)) where

import           Development.IDE             (IdeState, Recorder, WithPriority)
import           Ide.Plugin.Class.CodeAction
import           Ide.Plugin.Class.CodeLens
import           Ide.Plugin.Class.Types
import           Ide.Types
import           Language.LSP.Types

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
    { pluginCommands = commands plId
    , pluginRules = rules recorder
    , pluginHandlers = mkPluginHandler STextDocumentCodeAction (codeAction recorder)
        <> mkPluginHandler STextDocumentCodeLens codeLens
    , pluginConfigDescriptor =
        defaultConfigDescriptor { configCustomConfig = mkCustomConfig properties }
    }

commands :: PluginId -> [PluginCommand IdeState]
commands plId
  = [ PluginCommand codeActionCommandId
        "add placeholders for minimal methods" (addMethodPlaceholders plId)
    , PluginCommand typeLensCommandId
        "add type signatures for instance methods" codeLensCommandHandler
    ]
