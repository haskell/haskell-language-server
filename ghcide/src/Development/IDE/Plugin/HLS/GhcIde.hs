{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.GhcIde
  (
    descriptors
  , Log(..)
  ) where

import           Development.IDE
import qualified Development.IDE.LSP.HoverDefinition as Hover
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
  | LogHover Hover.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogNotifications msg -> pretty msg
    LogCompletions msg   -> pretty msg
    LogTypeLenses msg    -> pretty msg
    LogHover msg         -> pretty msg

descriptors :: Recorder (WithPriority Log) -> [PluginDescriptor IdeState]
descriptors recorder =
  [ descriptor (cmapWithPrio LogHover recorder) "ghcide-hover-and-symbols",
    Completions.descriptor (cmapWithPrio LogCompletions recorder) "ghcide-completions",
    TypeLenses.descriptor (cmapWithPrio LogTypeLenses recorder) "ghcide-type-lenses",
    Notifications.descriptor (cmapWithPrio LogNotifications recorder) "ghcide-core"
  ]

-- ---------------------------------------------------------------------

descriptor :: Recorder (WithPriority Hover.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId desc)
  { pluginHandlers = mkPluginHandler SMethod_TextDocumentHover (hover' recorder)
                  <> mkPluginHandler SMethod_TextDocumentDocumentSymbol moduleOutline
                  <> mkPluginHandler SMethod_TextDocumentDefinition (\ide _ DefinitionParams{..} ->
                      Hover.gotoDefinition recorder ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentTypeDefinition (\ide _ TypeDefinitionParams{..} ->
                      Hover.gotoTypeDefinition recorder ide TextDocumentPositionParams{..})
                --   <> mkPluginHandler SMethod_TextDocumentImplementation (\ide _ ImplementationParams{..} ->
                --       Hover.gotoImplementation recorder ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentDocumentHighlight (\ide _ DocumentHighlightParams{..} ->
                      Hover.documentHighlight recorder ide TextDocumentPositionParams{..})
                  <> mkPluginHandler SMethod_TextDocumentReferences (Hover.references recorder)
                  <> mkPluginHandler SMethod_WorkspaceSymbol (Hover.wsSymbols recorder),

    pluginConfigDescriptor = defaultConfigDescriptor
  }
  where
    desc = "Provides core IDE features for Haskell"

-- ---------------------------------------------------------------------

hover' :: Recorder (WithPriority Hover.Log) -> PluginMethodHandler IdeState Method_TextDocumentHover
hover' recorder ideState _ HoverParams{..} =
    Hover.hover recorder ideState TextDocumentPositionParams{..}
