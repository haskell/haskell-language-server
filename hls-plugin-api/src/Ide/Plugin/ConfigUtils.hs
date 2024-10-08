{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.ConfigUtils where

import           Control.Lens                  (at, (&), (?~))
import qualified Data.Aeson                    as A
import           Data.Aeson.Lens               (_Object)
import qualified Data.Aeson.Types              as A
import           Data.Default
import qualified Data.Dependent.Map            as DMap
import qualified Data.Dependent.Sum            as DSum
import           Data.List.Extra               (nubOrd)
import           Data.String                   (IsString (fromString))
import qualified Data.Text                     as T
import           GHC.TypeLits                  (symbolVal)
import           Ide.Plugin.Config
import           Ide.Plugin.Properties         (MetaData (..), Properties (..),
                                                toDefaultJSON,
                                                toVSCodeExtensionSchema)
import           Ide.Types
import           Language.LSP.Protocol.Message

-- Attention:
-- 'diagnosticsOn' will never be added into the default config or the schema,
-- since diagnostics emit in arbitrary shake rules -- we don't know
-- whether a plugin is capable of producing diagnostics.

-- | Generates a default 'Config', but remains only effective items
pluginsToDefaultConfig :: IdePlugins a -> A.Value
pluginsToDefaultConfig IdePlugins {..} =
  -- Use '_Object' and 'at' to get at the "plugin" key
  -- and actually set it.
  A.toJSON defaultConfig & _Object . at "plugin" ?~ elems
  where
    defaultConfig@Config {} = def
    elems = A.object $ mconcat $ singlePlugin <$> ipMap
    -- Splice genericDefaultConfig and dedicatedDefaultConfig
    -- Example:
    --
    -- {
    --  "plugin-id": {
    --    "globalOn": true,
    --    "codeActionsOn": true,
    --    "codeLensOn": true,
    --    "config": {
    --      "property1": "foo"
    --     }
    --   }
    -- }
    singlePlugin PluginDescriptor {pluginConfigDescriptor = ConfigDescriptor {..}, ..} =
      let x = genericDefaultConfig <> dedicatedDefaultConfig
       in [fromString (T.unpack pId) A..= A.object x | not $ null x]
      where
        (PluginHandlers (DMap.toList -> handlers)) = pluginHandlers
        customConfigToDedicatedDefaultConfig (CustomConfig p) = toDefaultJSON p
        -- Example:
        --
        -- {
        --   "codeActionsOn": true,
        --   "codeLensOn": true
        -- }
        --
        genericDefaultConfig =
            let x = ["diagnosticsOn" A..= True | configHasDiagnostics]
                        <> nubOrd (mconcat
                            (handlersToGenericDefaultConfig configInitialGenericConfig <$> handlers))
            in case x of
                    -- if the plugin has only one capability, we produce globalOn instead of the specific one;
                    -- otherwise we don't produce globalOn at all
                    [_] -> ["globalOn" A..= plcGlobalOn configInitialGenericConfig]
                    _   -> x
        -- Example:
        --
        -- {
        --  "config": {
        --      "property1": "foo"
        --   }
        --}
        dedicatedDefaultConfig =
          let x = customConfigToDedicatedDefaultConfig configCustomConfig
           in ["config" A..= A.object x | not $ null x]

        (PluginId pId) = pluginId

        -- This function captures ide methods registered by the plugin, and then converts it to kv pairs
        handlersToGenericDefaultConfig :: PluginConfig -> DSum.DSum IdeMethod f -> [A.Pair]
        handlersToGenericDefaultConfig PluginConfig{..} (IdeMethod m DSum.:=> _) = case m of
          SMethod_TextDocumentCodeAction           -> ["codeActionsOn" A..= plcCodeActionsOn]
          SMethod_TextDocumentCodeLens             -> ["codeLensOn" A..= plcCodeLensOn]
          SMethod_TextDocumentInlayHint            -> ["inlayHintsOn" A..= plcInlayHintsOn]
          SMethod_TextDocumentRename               -> ["renameOn" A..= plcRenameOn]
          SMethod_TextDocumentHover                -> ["hoverOn" A..= plcHoverOn]
          SMethod_TextDocumentDocumentSymbol       -> ["symbolsOn" A..= plcSymbolsOn]
          SMethod_TextDocumentCompletion           -> ["completionOn" A..= plcCompletionOn]
          SMethod_TextDocumentPrepareCallHierarchy -> ["callHierarchyOn" A..= plcCallHierarchyOn]
          SMethod_TextDocumentSemanticTokensFull   -> ["semanticTokensOn" A..= plcSemanticTokensOn]
          SMethod_TextDocumentSemanticTokensFullDelta -> ["semanticTokensOn" A..= plcSemanticTokensOn]
          _                                 -> []

-- | Generates json schema used in haskell vscode extension
-- Similar to 'pluginsToDefaultConfig' but simpler, since schema has a flatten structure
pluginsToVSCodeExtensionSchema :: IdePlugins a -> A.Value
pluginsToVSCodeExtensionSchema IdePlugins {..} = A.object $ mconcat $ singlePlugin <$> ipMap
  where
    singlePlugin PluginDescriptor {pluginConfigDescriptor = ConfigDescriptor {..}, ..} = genericSchema <> dedicatedSchema
      where
        (PluginHandlers (DMap.toList -> handlers)) = pluginHandlers
        customConfigToDedicatedSchema (CustomConfig p) = toVSCodeExtensionSchema (withIdPrefix "config.") p
        (PluginId pId) = pluginId
        genericSchema =
          let x =
                [toKey' "diagnosticsOn" A..= schemaEntry "diagnostics" True | configHasDiagnostics]
                  <> nubOrd (mconcat (handlersToGenericSchema configInitialGenericConfig <$> handlers))
           in case x of
                -- If the plugin has only one capability, we produce globalOn instead of the specific one;
                -- otherwise we don't produce globalOn at all
                [_] -> [toKey' "globalOn" A..= schemaEntry "plugin" (plcGlobalOn configInitialGenericConfig)]
                _   -> x
        dedicatedSchema = customConfigToDedicatedSchema configCustomConfig
        handlersToGenericSchema PluginConfig{..} (IdeMethod m DSum.:=> _) = case m of
          SMethod_TextDocumentCodeAction           -> [toKey' "codeActionsOn" A..= schemaEntry "code actions" plcCodeActionsOn]
          SMethod_TextDocumentCodeLens             -> [toKey' "codeLensOn" A..= schemaEntry "code lenses" plcCodeLensOn]
          SMethod_TextDocumentInlayHint            -> [toKey' "inlayHintsOn" A..= schemaEntry "inlay hints" plcInlayHintsOn]
          SMethod_TextDocumentRename               -> [toKey' "renameOn" A..= schemaEntry "rename" plcRenameOn]
          SMethod_TextDocumentHover                -> [toKey' "hoverOn" A..= schemaEntry "hover" plcHoverOn]
          SMethod_TextDocumentDocumentSymbol       -> [toKey' "symbolsOn" A..= schemaEntry "symbols" plcSymbolsOn]
          SMethod_TextDocumentCompletion           -> [toKey' "completionOn" A..= schemaEntry "completions" plcCompletionOn]
          SMethod_TextDocumentPrepareCallHierarchy -> [toKey' "callHierarchyOn" A..= schemaEntry "call hierarchy" plcCallHierarchyOn]
          SMethod_TextDocumentSemanticTokensFull   -> [toKey' "semanticTokensOn" A..= schemaEntry "semantic tokens" plcSemanticTokensOn]
          SMethod_TextDocumentSemanticTokensFullDelta   -> [toKey' "semanticTokensOn" A..= schemaEntry "semantic tokens" plcSemanticTokensOn]
          _                                        -> []
        schemaEntry desc defaultVal =
          A.object
            [ "scope" A..= A.String "resource",
              "type" A..= A.String "boolean",
              "default" A..= A.Bool defaultVal,
              "description" A..= A.String ("Enables " <> pId <> " " <> desc)
            ]
        withIdPrefix x = "haskell.plugin." <> pId <> "." <> x
        toKey' = fromString . T.unpack . withIdPrefix

data PluginCustomConfig = PluginCustomConfig {
    pccHeader :: T.Text,
    pccParams :: [PluginCustomConfigParam]
}
data PluginCustomConfigParam = PluginCustomConfigParam {
    pccpName        :: T.Text,
    pccpDescription :: T.Text,
    pccpIsDefault   :: Bool
}

-- | Generates markdown tables for custom config
pluginsCustomConfigToMarkdownTables :: IdePlugins a -> T.Text
pluginsCustomConfigToMarkdownTables IdePlugins {..} = T.unlines
    $ map renderCfg
    $ filter (\(PluginCustomConfig _ params) -> not $ null params)
    $ map pluginCfg ipMap
  where
    renderCfg :: PluginCustomConfig -> T.Text
    renderCfg (PluginCustomConfig pId pccParams) =
        T.unlines (pluginHeader : tableHeader : rows pccParams)
        where
            pluginHeader = "## " <> pId
            tableHeader = "| Property | Description | Default |" <> "\n" <> "| --- | --- | --- |"
            rows = map renderRow
            renderRow (PluginCustomConfigParam name desc isDefault) =
                "| `" <> name <> "` | " <> desc <> " | " <> if isDefault then "Yes" else "No" <> " |"
    pluginCfg :: PluginDescriptor r -> PluginCustomConfig
    pluginCfg PluginDescriptor {pluginConfigDescriptor = ConfigDescriptor {configCustomConfig = c}, pluginId = PluginId pId} =
        PluginCustomConfig pId (pccProcess c)
        where
            pccProcess :: CustomConfig -> [PluginCustomConfigParam]
            pccProcess (CustomConfig EmptyProperties) = mempty
            pccProcess (CustomConfig (ConsProperties keyNameProxy _k m xs)) =
                let (desc, isDefault) = case m of
                        PropertiesMetaData _ desc _ -> (desc, False)
                        EnumMetaData _ desc  _ _    -> (desc, True)
                        MetaData _ desc             -> (desc, False)
                in PluginCustomConfigParam (T.pack $ symbolVal keyNameProxy) desc isDefault : pccProcess (CustomConfig xs)
