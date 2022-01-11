{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.GhcIde
  (
    descriptors
  , Log(..)
  , logToPriority) where
import           Control.Monad.IO.Class
import           Development.IDE
import           Development.IDE.LSP.HoverDefinition
import qualified Development.IDE.LSP.Notifications   as Notifications
import           Development.IDE.LSP.Outline
import qualified Development.IDE.Plugin.CodeAction   as CodeAction
import qualified Development.IDE.Plugin.Completions  as Completions
import qualified Development.IDE.Plugin.TypeLenses   as TypeLenses
import qualified Development.IDE.Types.Logger        as Logger
import           Ide.Types
import           Language.LSP.Server                 (LspM)
import           Language.LSP.Types
import           Prettyprinter                       (Pretty (pretty))
import           Text.Regex.TDFA.Text                ()

data Log
  = LogNotifications Notifications.Log
  | LogCompletions Completions.Log
  | LogTypeLenses TypeLenses.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogNotifications log -> pretty log
    LogCompletions log   -> pretty log
    LogTypeLenses log    -> pretty log

logToPriority :: Log -> Logger.Priority
logToPriority = \case
  LogNotifications log -> Notifications.logToPriority log
  LogCompletions log   -> Completions.logToPriority log
  LogTypeLenses log    -> TypeLenses.logToPriority log

descriptors :: Recorder Log -> [PluginDescriptor IdeState]
descriptors recorder =
  [ descriptor "ghcide-hover-and-symbols",
    CodeAction.iePluginDescriptor "ghcide-code-actions-imports-exports",
    CodeAction.typeSigsPluginDescriptor "ghcide-code-actions-type-signatures",
    CodeAction.bindingsPluginDescriptor "ghcide-code-actions-bindings",
    CodeAction.fillHolePluginDescriptor "ghcide-code-actions-fill-holes",
    Completions.descriptor (cmap LogCompletions recorder) "ghcide-completions",
    TypeLenses.descriptor (cmap LogTypeLenses recorder) "ghcide-type-lenses",
    Notifications.descriptor (cmap LogNotifications recorder) "ghcide-core"
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
