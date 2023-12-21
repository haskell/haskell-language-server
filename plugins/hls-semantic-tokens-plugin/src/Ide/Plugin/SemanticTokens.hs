{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.SemanticTokens (descriptor) where

import           Development.IDE
import           Development.IDE.Core.Rules         (Log)
import qualified Ide.Plugin.SemanticTokens.Internal as Internal
import qualified Ide.Plugin.SemanticTokens.Types    as Types
import           Ide.Types
import           Language.LSP.Protocol.Message



descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "provides lsp semantic tokens features")
    { Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentSemanticTokensFull Internal.semanticTokensFull
      , Ide.Types.pluginRules = Internal.getImportedNameSemanticRule recorder
    }


