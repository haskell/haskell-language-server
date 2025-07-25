{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}
module Ide.Plugin.Config
    ( getConfigFromNotification
    , Config(..)
    , parseConfig
    , PluginConfig(..)
    , CheckParents(..)
    ) where

import           Control.Lens     (preview)
import           Data.Aeson       hiding (Error)
import qualified Data.Aeson       as A
import           Data.Aeson.Lens  (_String)
import qualified Data.Aeson.Types as A
import           Data.Default
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (fromMaybe)
import qualified Data.Text        as T
import           GHC.Exts         (toList)
import           Ide.Types

-- ---------------------------------------------------------------------

-- | Given a DidChangeConfigurationNotification message, this function returns the parsed
-- Config object if possible.
getConfigFromNotification :: IdePlugins s -> Config -> A.Value -> Either T.Text Config
getConfigFromNotification plugins defaultValue p =
  case A.parse (parseConfig plugins defaultValue) p of
    A.Success c -> Right c
    A.Error err -> Left $ T.pack err

-- ---------------------------------------------------------------------

parseConfig :: IdePlugins s -> Config -> Value -> A.Parser Config
parseConfig idePlugins defValue = A.withObject "settings" $ \o ->
  Config
    <$> o .:? "checkParents"                            .!= checkParents defValue
    <*> o .:? "checkProject"                            .!= checkProject defValue
    <*> o .:? "formattingProvider"                      .!= formattingProvider defValue
    <*> o .:? "cabalFormattingProvider"                 .!= cabalFormattingProvider defValue
    <*> o .:? "maxCompletions"                          .!= maxCompletions defValue
    <*> o .:? "sessionLoading"                          .!= sessionLoading defValue
    <*> A.explicitParseFieldMaybe (parsePlugins idePlugins) o "plugin" .!= plugins defValue

-- | Parse the 'PluginConfig'.
--   Since we need to fall back to default values if we do not find one in the input,
--   we need the map of plugin-provided defaults, as in 'parseConfig'.
parsePlugins :: IdePlugins s -> Value -> A.Parser (Map.Map PluginId PluginConfig)
parsePlugins (IdePlugins plugins) = A.withObject "Config.plugins" $ \o -> do
  let -- parseOne :: Key -> Value -> A.Parser (T.Text, PluginConfig)
      parseOne (fmap PluginId . preview _String . toJSON -> Just pId) pConfig = do
        let defPluginConfig = fromMaybe def $ lookup pId defValue
        pConfig' <- parsePluginConfig defPluginConfig pConfig
        return (pId, pConfig')
      parseOne _ _ = fail "Expected plugin id to be a string"
      defValue = map (\p -> (pluginId p, configInitialGenericConfig (pluginConfigDescriptor p))) plugins
  plugins <- mapM (uncurry parseOne) (toList o)
  return $ Map.fromList plugins

-- ---------------------------------------------------------------------

parsePluginConfig :: PluginConfig -> Value -> A.Parser PluginConfig
parsePluginConfig def = A.withObject "PluginConfig" $ \o -> PluginConfig
      <$> o .:? "globalOn"         .!= plcGlobalOn def
      <*> o .:? "callHierarchyOn"  .!= plcCallHierarchyOn def
      <*> o .:? "codeActionsOn"    .!= plcCodeActionsOn def
      <*> o .:? "codeLensOn"       .!= plcCodeLensOn    def
      <*> o .:? "inlayHintsOn"     .!= plcInlayHintsOn  def
      <*> o .:? "diagnosticsOn"    .!= plcDiagnosticsOn def -- AZ
      <*> o .:? "hoverOn"          .!= plcHoverOn       def
      <*> o .:? "symbolsOn"        .!= plcSymbolsOn     def
      <*> o .:? "signatureHelpOn"  .!= plcSignatureHelpOn def
      <*> o .:? "completionOn"     .!= plcCompletionOn  def
      <*> o .:? "renameOn"         .!= plcRenameOn      def
      <*> o .:? "selectionRangeOn" .!= plcSelectionRangeOn def
      <*> o .:? "foldingRangeOn"   .!= plcFoldingRangeOn def
      <*> o .:? "semanticTokensOn" .!= plcSemanticTokensOn def
      <*> o .:? "config"           .!= plcConfig        def

-- ---------------------------------------------------------------------
