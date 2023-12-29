{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.SemanticTokens (descriptor) where

import           Development.IDE
import           Development.IDE.Core.Rules         (Log)
import qualified Ide.Plugin.SemanticTokens.Internal as Internal
import           Ide.Types
import           Language.LSP.Protocol.Message



descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Provides semantic tokens")
    { Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentSemanticTokensFull Internal.semanticTokensFull
      , Ide.Types.pluginRules =
        Internal.getSemanticTokensRule recorder
        <> Internal.persistentGetSemanticTokensRule
    }


