module Ide.Plugin.CallHierarchy (descriptor) where

import           Development.IDE
import qualified Ide.Plugin.CallHierarchy.Internal as X
import           Ide.Types
import           Language.LSP.Types

descriptor :: PluginDescriptor IdeState
descriptor = (defaultPluginDescriptor X.callHierarchyId)
    { Ide.Types.pluginHandlers =
        mkPluginHandler STextDocumentPrepareCallHierarchy X.prepareCallHierarchy
     <> mkPluginHandler SCallHierarchyIncomingCalls X.incomingCalls
     <> mkPluginHandler SCallHierarchyOutgoingCalls X.outgoingCalls
    }
