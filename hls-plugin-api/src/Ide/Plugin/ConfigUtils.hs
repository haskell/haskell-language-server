{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.ConfigUtils where

import qualified Data.Aeson            as A
import qualified Data.Aeson.Types      as A
import           Data.Default          (def)
import qualified Data.Dependent.Map    as DMap
import qualified Data.Dependent.Sum    as DSum
import qualified Data.HashMap.Lazy     as HMap
import           Data.List             (nub)
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
  A.Object $
    HMap.adjust
      ( \(unsafeValueToObject -> o) ->
          A.Object $ HMap.insert "plugin" elems o -- inplace the "plugin" section with our 'elems', leaving others unchanged
      )
      "haskell"
      (unsafeValueToObject (A.toJSON defaultConfig))
  where
    defaultConfig@Config {} = def
    unsafeValueToObject (A.Object o) = o
    unsafeValueToObject _            = error "impossible"
    elems = A.object $ mconcat $ singlePlugin <$> map snd ipMap
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
       in [pId A..= A.object x | not $ null x]
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
          let x = ["diagnosticsOn" A..= True | configHasDiagnostics] <> nub (mconcat (handlersToGenericDefaultConfig <$> handlers))
           in case x of
                -- if the plugin has only one capability, we produce globalOn instead of the specific one;
                -- otherwise we don't produce globalOn at all
                [_] -> ["globalOn" A..= True]
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
        handlersToGenericDefaultConfig :: DSum.DSum IdeMethod f -> [A.Pair]
        handlersToGenericDefaultConfig (IdeMethod m DSum.:=> _) = case m of
          STextDocumentCodeAction           -> ["codeActionsOn" A..= True]
          STextDocumentCodeLens             -> ["codeLensOn" A..= True]
          STextDocumentRename               -> ["renameOn" A..= True]
          STextDocumentHover                -> ["hoverOn" A..= True]
          STextDocumentDocumentSymbol       -> ["symbolsOn" A..= True]
          STextDocumentCompletion           -> ["completionOn" A..= True]
          STextDocumentPrepareCallHierarchy -> ["callHierarchyOn" A..= True]
          _                                 -> []

-- | Generates json schema used in haskell vscode extension
-- Similar to 'pluginsToDefaultConfig' but simpler, since schema has a flatten structure
pluginsToVSCodeExtensionSchema :: IdePlugins a -> A.Value
pluginsToVSCodeExtensionSchema IdePlugins {..} = A.object $ mconcat $ singlePlugin <$> map snd ipMap
  where
    singlePlugin PluginDescriptor {pluginConfigDescriptor = ConfigDescriptor {..}, ..} = genericSchema <> dedicatedSchema
      where
        (PluginHandlers (DMap.toList -> handlers)) = pluginHandlers
        customConfigToDedicatedSchema (CustomConfig p) = toVSCodeExtensionSchema (withIdPrefix "config.") p
        (PluginId pId) = pluginId
        genericSchema =
          let x =
                [withIdPrefix "diagnosticsOn" A..= schemaEntry "diagnostics" | configHasDiagnostics]
                  <> nub (mconcat (handlersToGenericSchema <$> handlers))
           in case x of
                -- If the plugin has only one capability, we produce globalOn instead of the specific one;
                -- otherwise we don't produce globalOn at all
                [_] -> [withIdPrefix "globalOn" A..= schemaEntry "plugin"]
                _   -> x
        dedicatedSchema = customConfigToDedicatedSchema configCustomConfig
        handlersToGenericSchema (IdeMethod m DSum.:=> _) = case m of
          STextDocumentCodeAction -> [withIdPrefix "codeActionsOn" A..= schemaEntry "code actions"]
          STextDocumentCodeLens -> [withIdPrefix "codeLensOn" A..= schemaEntry "code lenses"]
          STextDocumentRename -> [withIdPrefix "renameOn" A..= schemaEntry "rename"]
          STextDocumentHover -> [withIdPrefix "hoverOn" A..= schemaEntry "hover"]
          STextDocumentDocumentSymbol -> [withIdPrefix "symbolsOn" A..= schemaEntry "symbols"]
          STextDocumentCompletion -> [withIdPrefix "completionOn" A..= schemaEntry "completions"]
          STextDocumentPrepareCallHierarchy -> [withIdPrefix "callHierarchyOn" A..= schemaEntry "call hierarchy"]
          _ -> []
        schemaEntry desc =
          A.object
            [ "scope" A..= A.String "resource",
              "type" A..= A.String "boolean",
              "default" A..= True,
              "description" A..= A.String ("Enables " <> pId <> " " <> desc)
            ]
        withIdPrefix x = "haskell.plugin." <> pId <> "." <> x
