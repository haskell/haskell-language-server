{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.GhcIde
  (
    descriptor
  ) where

import Development.IDE.Core.Service
import Development.IDE.LSP.HoverDefinition
import Development.IDE.Types.Logger
import Ide.Types
import Text.Regex.TDFA.Text()
import Development.IDE.Plugin.CodeAction

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginRules = mempty
  , pluginCommands = []
  , pluginCodeActionProvider = Just codeAction'
  , pluginCodeLensProvider   = Just codeLens'
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

codeAction' :: CodeActionProvider
codeAction' lf ide _ doc range context = codeAction lf ide doc range context

-- ---------------------------------------------------------------------

codeLens' :: CodeLensProvider
codeLens' lf ide _ params = codeLens lf ide params

-- ---------------------------------------------------------------------
