{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.GhcIde
  (
    descriptor
  ) where

import Data.Aeson
import Development.IDE.Core.Service
import Development.IDE.LSP.HoverDefinition
import Development.IDE.LSP.Outline
import Development.IDE.Plugin.CodeAction
import Development.IDE.Plugin.Completions
import Development.IDE.Types.Logger
import Ide.Plugin
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
  , pluginSymbolsProvider    = Just symbolsProvider
  , pluginFormattingProvider = Nothing
  , pluginCompletionProvider = Just getCompletionsLSP
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
codeAction' lf ide _ doc range context = fmap List <$> codeAction lf ide doc range context

-- ---------------------------------------------------------------------

codeLens' :: CodeLensProvider
codeLens' lf ide _ params = codeLens lf ide params

-- ---------------------------------------------------------------------

symbolsProvider :: SymbolsProvider
symbolsProvider ls ide params = do
    ds <- moduleOutline ls ide params
    case ds of
        Right (DSDocumentSymbols (List ls)) -> return $ Right ls
        Right (DSSymbolInformation (List _si)) ->
            return $ Left $ responseError "GhcIde.symbolsProvider: DSSymbolInformation deprecated"
        Left err -> return $ Left err

-- ---------------------------------------------------------------------
