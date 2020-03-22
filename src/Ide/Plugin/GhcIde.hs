{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.GhcIde
  (
    descriptor
  ) where

import Data.Aeson
import Development.IDE.Core.Service
import Development.IDE.LSP.HoverDefinition
import Development.IDE.Plugin.CodeAction
import Development.IDE.Types.Logger
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginRules = mempty
  , pluginCommands = [PluginCommand (CommandId "typesignature.add") "adds a signature" commandAddSignature]
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

commandAddSignature :: CommandFunction WorkspaceEdit
commandAddSignature lf ide params
    = executeAddSignatureCommand lf ide (ExecuteCommandParams "typesignature.add" (Just (List [toJSON params])) Nothing)

-- ---------------------------------------------------------------------

codeAction' :: CodeActionProvider
codeAction' lf ide _ doc range context = codeAction lf ide doc range context

-- ---------------------------------------------------------------------

codeLens' :: CodeLensProvider
codeLens' lf ide _ params = codeLens lf ide params

-- ---------------------------------------------------------------------
