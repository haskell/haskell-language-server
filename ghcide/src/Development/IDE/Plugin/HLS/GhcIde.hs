{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.GhcIde
  (
    descriptors
  ) where
import           Control.Monad.IO.Class
import           Development.IDE
import           Development.IDE.LSP.HoverDefinition
import qualified Development.IDE.LSP.Notifications   as Notifications
import           Development.IDE.LSP.Outline
import qualified Development.IDE.Plugin.CodeAction   as CodeAction
import qualified Development.IDE.Plugin.Completions  as Completions
import qualified Development.IDE.Plugin.TypeLenses   as TypeLenses
import           Ide.Types
import           Language.LSP.Server                 (LspM)
import           Language.LSP.Types
import           Text.Regex.TDFA.Text                ()

descriptors :: [PluginDescriptor IdeState]
descriptors =
  [ descriptor "ghcide-hover-and-symbols",
    CodeAction.iePluginDescriptor "ghcide-code-actions-imports-exports",
    CodeAction.typeSigsPluginDescriptor "ghcide-code-actions-type-signatures",
    CodeAction.bindingsPluginDescriptor "ghcide-code-actions-bindings",
    CodeAction.fillHolePluginDescriptor "ghcide-code-actions-fill-holes",
    Completions.descriptor "ghcide-completions",
    TypeLenses.descriptor "ghcide-type-lenses",
    Notifications.descriptor "ghcide-core"
  ]

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentHover hover'
                  <> mkPluginHandler STextDocumentDocumentSymbol symbolsProvider,
    pluginConfigDescriptor = defaultConfigDescriptor {configEnableGenericConfig = False}
  }

-- ---------------------------------------------------------------------

hover' :: IdeState -> PluginId -> HoverParams  -> LspM c (Either ResponseError (Maybe Hover))
hover' ideState _ HoverParams{..} = do
    liftIO $ logDebug (ideLogger ideState) "GhcIde.hover entered (ideLogger)" -- AZ
    hover ideState TextDocumentPositionParams{..}

-- ---------------------------------------------------------------------
symbolsProvider :: IdeState -> PluginId -> DocumentSymbolParams -> LspM c (Either ResponseError (List DocumentSymbol |? List SymbolInformation))
symbolsProvider ide _ params = moduleOutline ide params

-- ---------------------------------------------------------------------
