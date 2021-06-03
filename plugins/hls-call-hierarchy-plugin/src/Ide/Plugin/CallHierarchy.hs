module Ide.Plugin.CallHierarchy where

import           Development.IDE
import           Ide.Types
import           Language.LSP.Types

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { Ide.Types.pluginHandlers = mkPluginHandler STextDocumentPrepareCallHierarchy prepareCallHierarchy
  }

prepareCallHierarchy :: PluginMethodHandler IdeState TextDocumentPrepareCallHierarchy
prepareCallHierarchy = undefined

incomingCalls :: PluginMethodHandler IdeState CallHierarchyIncomingCalls
incomingCalls = undefined

outgoingCalls :: PluginMethodHandler IdeState CallHierarchyOutgoingCalls
outgoingCalls = undefined
