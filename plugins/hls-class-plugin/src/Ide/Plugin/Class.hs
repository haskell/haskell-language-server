module Ide.Plugin.Class (descriptor, Log(..)) where

import           Development.IDE               (IdeState, Recorder,
                                                WithPriority)
import           Ide.Plugin.Class.CodeAction
import           Ide.Plugin.Class.CodeLens
import           Ide.Plugin.Class.Types
import           Ide.Types
import           Language.LSP.Protocol.Message
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Provides code actions and lenses for working with typeclasses")
    { pluginCommands = commands plId
    , pluginRules = getInstanceBindTypeSigsRule recorder >> getInstanceBindLensRule recorder
    , pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeAction (codeAction recorder)
        <> mkPluginHandler SMethod_TextDocumentCodeLens codeLens
        <> mkResolveHandler SMethod_CodeLensResolve codeLensResolve
    }

commands :: PluginId -> [PluginCommand IdeState]
commands plId
  = [ PluginCommand codeActionCommandId
        "add placeholders for minimal methods" (addMethodPlaceholders plId)
    , PluginCommand typeLensCommandId
        "add type signatures for instance methods" (codeLensCommandHandler plId)
    ]
