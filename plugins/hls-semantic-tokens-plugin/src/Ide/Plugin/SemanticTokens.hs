module Ide.Plugin.SemanticTokens (descriptor, Internal.recoverSemanticTokens) where

import           Development.IDE
import qualified Ide.Plugin.SemanticTokens.Internal as Internal
import qualified Ide.Plugin.SemanticTokens.Types    as Types
import           Ide.Types
import           Language.LSP.Protocol.Message



descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
    { Ide.Types.pluginHandlers =
        mkPluginHandler SMethod_TextDocumentSemanticTokensFull Internal.semanticTokensFull
    }
