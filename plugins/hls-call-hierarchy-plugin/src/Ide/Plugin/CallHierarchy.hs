module Ide.Plugin.CallHierarchy where

import qualified Ide.Plugin.CallHierarchy.Internal as X

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { Ide.Types.pluginHandlers = mkPluginHandler STextDocumentPrepareCallHierarchy X.prepareCallHierarchy
                            <> mkPluginHandler SCallHierarchyIncomingCalls X.incomingCalls
  }
