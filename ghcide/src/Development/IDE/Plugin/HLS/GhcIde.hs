{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.GhcIde
  (
    descriptors
  , Log(..)
  ) where
import           Control.Monad.IO.Class
import           Development.IDE
import           Development.IDE.LSP.HoverDefinition
import qualified Development.IDE.LSP.Notifications   as Notifications
import           Development.IDE.LSP.Outline
import qualified Development.IDE.Plugin.Completions  as Completions
import qualified Development.IDE.Plugin.TypeLenses   as TypeLenses
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Text.Regex.TDFA.Text                ()

data Log
  = LogNotifications Notifications.Log
  | LogCompletions Completions.Log
  | LogTypeLenses TypeLenses.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogNotifications msg -> pretty msg
    LogCompletions msg   -> pretty msg
    LogTypeLenses msg    -> pretty msg

descriptors :: Recorder (WithPriority Log) -> [PluginDescriptor IdeState]
descriptors recorder =
  [ descriptor "ghcide-hover-and-symbols",
    Completions.descriptor (cmapWithPrio LogCompletions recorder) "ghcide-completions",
    TypeLenses.descriptor (cmapWithPrio LogTypeLenses recorder) "ghcide-type-lenses",
    Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"
  ]

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId desc)
  { pluginHandlers = mkPluginHandler SMethod_TextDocumentHover hover'
                  <> mkPluginHandler SMethod_TextDocumentDocumentSymbol moduleOutline
                  <> mkPluginHandler SMethod_TextDocumentDefinition (\ide _ DefinitionParams{..} ->
                      gotoDefinition ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentTypeDefinition (\ide _ TypeDefinitionParams{..} ->
                      gotoTypeDefinition ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentDocumentHighlight (\ide _ DocumentHighlightParams{..} ->
                      documentHighlight ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentReferences references
                  <> mkPluginHandler SMethod_WorkspaceSymbol wsSymbols,

    pluginConfigDescriptor = defaultConfigDescriptor
  }
  where
    desc = "Provides core IDE features for Haskell"

-- ---------------------------------------------------------------------

hover' :: PluginMethodHandler IdeState Method_TextDocumentHover
hover' ideState _ HoverParams{..} = do
    liftIO $ logDebug (ideLogger ideState) "GhcIde.hover entered (ideLogger)" -- AZ
    hover ideState TextDocumentPositionParams{..}
