{-# LANGUAGE OverloadedStrings #-}


module Ide.Plugin.SemanticTokens (descriptor) where

import           Development.IDE
import qualified Ide.Plugin.SemanticTokens.Internal as Internal
import           Ide.Plugin.SemanticTokens.Types
import           Ide.Types
import           Language.LSP.Protocol.Message

descriptor :: Recorder (WithPriority SemanticLog) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Provides semantic tokens")
    { Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentSemanticTokensFull (Internal.semanticTokensFull recorder)
        <> mkPluginHandler SMethod_TextDocumentSemanticTokensFullDelta (Internal.semanticTokensFullDelta recorder),
      Ide.Types.pluginRules = Internal.getSemanticTokensRule recorder <> Internal.getSyntacticTokensRule recorder,
      pluginConfigDescriptor =
        defaultConfigDescriptor
          { configInitialGenericConfig = (configInitialGenericConfig defaultConfigDescriptor) {plcGlobalOn = True}
          , configCustomConfig = mkCustomConfig Internal.semanticConfigProperties
          }
    }
