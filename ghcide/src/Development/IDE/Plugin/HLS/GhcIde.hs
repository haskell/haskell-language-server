{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.GhcIde
  (
    descriptors
  ) where
import Development.IDE
import Development.IDE.LSP.HoverDefinition
import Development.IDE.LSP.Outline
import Ide.PluginUtils
import Ide.Types
import Language.LSP.Types
import Language.LSP.Server (LspM)
import Text.Regex.TDFA.Text()
import qualified Development.IDE.Plugin.CodeAction as CodeAction
import qualified Development.IDE.Plugin.Completions as Completions
import qualified Development.IDE.Plugin.TypeLenses as TypeLenses
import Control.Monad.IO.Class

descriptors :: [PluginDescriptor IdeState]
descriptors =
  [ descriptor "ghcide-hover-and-symbols",
    CodeAction.descriptor "ghcide-code-actions",
    Completions.descriptor "ghcide-completions",
    TypeLenses.descriptor "ghcide-type-lenses"
  ]

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentHover hover'
                  <> mkPluginHandler STextDocumentDocumentSymbol symbolsProvider
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
