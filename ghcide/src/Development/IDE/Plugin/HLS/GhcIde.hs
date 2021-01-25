{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()
import qualified Development.IDE.Plugin.CodeAction as CodeAction
import qualified Development.IDE.Plugin.Completions as Completions
import qualified Development.IDE.Plugin.TypeLenses as TypeLenses

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
  { pluginHoverProvider      = Just hover'
  , pluginSymbolsProvider    = Just symbolsProvider
  }

-- ---------------------------------------------------------------------

hover' :: HoverProvider IdeState
hover' ideState params = do
    logDebug (ideLogger ideState) "GhcIde.hover entered (ideLogger)" -- AZ
    hover ideState params

-- ---------------------------------------------------------------------
symbolsProvider :: SymbolsProvider IdeState
symbolsProvider ls ide params = do
    ds <- moduleOutline ls ide params
    case ds of
        Right (DSDocumentSymbols (List ls)) -> return $ Right ls
        Right (DSSymbolInformation (List _si)) ->
            return $ Left $ responseError "GhcIde.symbolsProvider: DSSymbolInformation deprecated"
        Left err -> return $ Left err

-- ---------------------------------------------------------------------
