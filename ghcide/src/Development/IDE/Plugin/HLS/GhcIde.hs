{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.GhcIde
  (
    descriptor
  ) where
import Development.IDE
import Development.IDE.Plugin.Completions as Completions
import Development.IDE.Plugin.CodeAction as CodeAction
import Development.IDE.LSP.HoverDefinition
import Development.IDE.LSP.Outline
import Ide.PluginUtils
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCodeActionProvider = Just codeAction'
  , pluginHoverProvider      = Just hover'
  , pluginSymbolsProvider    = Just symbolsProvider
  , pluginCompletionProvider = Just getCompletionsLSP
  , pluginRules              = produceCompletions <> rulePackageExports
  }

-- ---------------------------------------------------------------------

hover' :: HoverProvider IdeState
hover' ideState params = do
    logDebug (ideLogger ideState) "GhcIde.hover entered (ideLogger)" -- AZ
    hover ideState params

codeAction' :: CodeActionProvider IdeState
codeAction' lf ide _ doc range context = fmap List <$> codeAction lf ide doc range context

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
