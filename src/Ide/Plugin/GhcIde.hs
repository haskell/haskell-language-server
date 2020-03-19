{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.GhcIde
  (
    descriptor
  ) where

import Development.IDE.Types.Logger
import Ide.Types
import Development.IDE.LSP.HoverDefinition
import Development.IDE.Core.Shake

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginRules = mempty
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Just hover'
  , pluginSymbolsProvider    = Nothing
  , pluginFormattingProvider = Nothing
  , pluginCompletionProvider = Nothing
  }

-- ---------------------------------------------------------------------

hover' :: HoverProvider
hover' ideState params = do
    logInfo (ideLogger ideState) "GhcIde.hover entered (ideLogger)" -- AZ
    hover ideState params

-- ---------------------------------------------------------------------
