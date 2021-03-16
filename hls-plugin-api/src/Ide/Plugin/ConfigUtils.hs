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
import qualified Data.Map              as Map
import           Ide.Plugin.Config
import           Ide.Plugin.Properties (toDefaultJSON, toVSCodeExtensionSchema)
import           Ide.Types
import           Language.LSP.Types

pluginsToDefaultConfig :: IdePlugins a -> A.Value
pluginsToDefaultConfig IdePlugins {..} =
  A.Object $
    HMap.adjust
      ( \(unsafeValueToObject -> o) ->
          A.Object $ HMap.insert "plugin" elems o
      )
      "haskell"
      (unsafeValueToObject (A.toJSON defaultConfig))
  where
    defaultConfig@Config {} = def
    unsafeValueToObject (A.Object o) = o
    unsafeValueToObject _            = error "impossible"
    elems = A.object $ mconcat $ singlePlugin <$> Map.elems ipMap
    singlePlugin PluginDescriptor {..} =
      let x = geenericDefaultConfig <> dedicatedDefaultConfig
       in [pId A..= A.object x | not $ null x]
      where
        (PluginHandlers (DMap.toList -> handlers)) = pluginHandlers
        customConfigToDedicatedDefaultConfig (CustomConfig p) = toDefaultJSON p
        -- we don't generate the config section if the plugin doesn't register any of the following six methods,
        -- which avoids producing redundant configuration for formatters:
        --
        -- "stylish-haskell": {
        --    "globalOn": true
        -- }
        geenericDefaultConfig =
          let x = mconcat (handlersToGenericDefaultConfig <$> handlers)
           in ["globalOn" A..= True | not $ null x] <> x
        dedicatedDefaultConfig =
          let x = customConfigToDedicatedDefaultConfig pluginCustomConfig
           in ["config" A..= A.object x | not $ null x]
        (PluginId pId) = pluginId
        handlersToGenericDefaultConfig :: DSum.DSum IdeMethod f -> [A.Pair]
        handlersToGenericDefaultConfig (IdeMethod m DSum.:=> _) = case m of
          STextDocumentCodeAction     -> ["codeActionsOn" A..= True]
          STextDocumentCodeLens       -> ["codeLensOn" A..= True]
          STextDocumentRename         -> ["renameOn" A..= True]
          STextDocumentHover          -> ["hoverOn" A..= True]
          STextDocumentDocumentSymbol -> ["symbolsOn" A..= True]
          STextDocumentCompletion     -> ["completionOn" A..= True]
          _                           -> []

pluginsToVSCodeExtensionSchema :: IdePlugins a -> A.Value
pluginsToVSCodeExtensionSchema IdePlugins {..} = A.object $ mconcat $ singlePlugin <$> Map.elems ipMap
  where
    singlePlugin PluginDescriptor {..} = genericSchema <> dedicatedSchema
      where
        (PluginHandlers (DMap.toList -> handlers)) = pluginHandlers
        customConfigToDedicatedSchema (CustomConfig p) = toVSCodeExtensionSchema (withIdPrefix "config.") p
        (PluginId pId) = pluginId
        genericSchema = withIdPrefix "globalOn" A..= schemaEntry "plugin" : mconcat (handlersToGenericSchema <$> handlers)
        dedicatedSchema = customConfigToDedicatedSchema pluginCustomConfig
        handlersToGenericSchema (IdeMethod m DSum.:=> _) = case m of
          STextDocumentCodeAction -> [withIdPrefix "codeActionsOn" A..= schemaEntry "code actions"]
          STextDocumentCodeLens -> [withIdPrefix "codeLensOn" A..= schemaEntry "code lenses"]
          STextDocumentRename -> [withIdPrefix "renameOn" A..= schemaEntry "rename"]
          STextDocumentHover -> [withIdPrefix "hoverOn" A..= schemaEntry "hover"]
          STextDocumentDocumentSymbol -> [withIdPrefix "symbolsOn" A..= schemaEntry "symbols"]
          STextDocumentCompletion -> [withIdPrefix "completionOn" A..= schemaEntry "completions"]
          _ -> []
        schemaEntry desc =
          A.object
            [ "scope" A..= A.String "resource",
              "type" A..= A.String "boolean",
              "default" A..= True,
              "description" A..= A.String ("Enables " <> pId <> " " <> desc)
            ]
        withIdPrefix x = "haskell.plugin." <> pId <> "." <> x
