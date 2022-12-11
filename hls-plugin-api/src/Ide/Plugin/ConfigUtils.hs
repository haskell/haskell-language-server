{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.ConfigUtils where

import           Control.Lens          (at, ix, (&), (?~))
import qualified Data.Aeson            as A
import           Data.Aeson.Lens       (_Object)
import qualified Data.Aeson.Types      as A
import           Data.Default
import qualified Data.Dependent.Map    as DMap
import qualified Data.Dependent.Sum    as DSum
import           Data.List.Extra       (nubOrd)
import           Data.String           (IsString (fromString))
import qualified Data.Text             as T
import           Ide.Plugin.Config
import           Ide.Plugin.Properties (toDefaultJSON, toVSCodeExtensionSchema)
import           Ide.Types
import           Language.LSP.Types

-- Attention:
-- 'diagnosticsOn' will never be added into the default config or the schema,
-- since diagnostics emit in arbitrary shake rules -- we don't know
-- whether a plugin is capable of producing diagnostics.

-- | Generates a default 'Config', but remains only effective items
pluginsToDefaultConfig :: IdePlugins a -> A.Value
pluginsToDefaultConfig IdePlugins {..} =
  -- Use 'ix' to look at all the "haskell" keys in the outer value (since we're not
  -- setting it if missing), then we use '_Object' and 'at' to get at the "plugin" key
  -- and actually set it.
  A.toJSON defaultConfig & ix "haskell" . _Object . at "plugin" ?~ elems
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
          STextDocumentCodeAction           -> ["codeActionsOn" A..= plcCodeActionsOn]
          STextDocumentCodeLens             -> ["codeLensOn" A..= plcCodeLensOn]
          STextDocumentRename               -> ["renameOn" A..= plcRenameOn]
          STextDocumentHover                -> ["hoverOn" A..= plcHoverOn]
          STextDocumentDocumentSymbol       -> ["symbolsOn" A..= plcSymbolsOn]
          STextDocumentCompletion           -> ["completionOn" A..= plcCompletionOn]
          STextDocumentPrepareCallHierarchy -> ["callHierarchyOn" A..= plcCallHierarchyOn]
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
                [toKey' "diagnosticsOn" A..= schemaEntry "diagnostics" | configHasDiagnostics]
                  <> nubOrd (mconcat (handlersToGenericSchema <$> handlers))
           in case x of
                -- If the plugin has only one capability, we produce globalOn instead of the specific one;
                -- otherwise we don't produce globalOn at all
                [_] -> [toKey' "globalOn" A..= schemaEntry "plugin"]
                _   -> x
        dedicatedSchema = customConfigToDedicatedSchema configCustomConfig
        handlersToGenericSchema (IdeMethod m DSum.:=> _) = case m of
          STextDocumentCodeAction -> [toKey' "codeActionsOn" A..= schemaEntry "code actions"]
          STextDocumentCodeLens -> [toKey' "codeLensOn" A..= schemaEntry "code lenses"]
          STextDocumentRename -> [toKey' "renameOn" A..= schemaEntry "rename"]
          STextDocumentHover -> [toKey' "hoverOn" A..= schemaEntry "hover"]
          STextDocumentDocumentSymbol -> [toKey' "symbolsOn" A..= schemaEntry "symbols"]
          STextDocumentCompletion -> [toKey' "completionOn" A..= schemaEntry "completions"]
          STextDocumentPrepareCallHierarchy -> [toKey' "callHierarchyOn" A..= schemaEntry "call hierarchy"]
          _ -> []
        schemaEntry desc =
          A.object
            [ "scope" A..= A.String "resource",
              "type" A..= A.String "boolean",
              "default" A..= True,
              "description" A..= A.String ("Enables " <> pId <> " " <> desc)
            ]
        withIdPrefix x = "haskell.plugin." <> pId <> "." <> x
        toKey' = fromString . T.unpack . withIdPrefix
