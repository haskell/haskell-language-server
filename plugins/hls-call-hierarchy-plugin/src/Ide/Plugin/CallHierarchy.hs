{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.CallHierarchy (descriptor) where

import           Development.IDE
import qualified Ide.Plugin.CallHierarchy.Internal as X
import           Ide.Types
import           Language.LSP.Protocol.Message

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId "Provides call-hierarchy support in Haskell")
    { Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentPrepareCallHierarchy X.prepareCallHierarchy
     <> mkPluginHandler SMethod_CallHierarchyIncomingCalls X.incomingCalls
     <> mkPluginHandler SMethod_CallHierarchyOutgoingCalls X.outgoingCalls
    }
