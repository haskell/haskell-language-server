{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.ConfigUtils where

import qualified Data.Aeson            as A
import qualified Data.Dependent.Map    as DMap
import qualified Data.Dependent.Sum    as DSum
import qualified Data.Map              as Map
import           Ide.Plugin.Properties (toVSCodeExtensionSchema)
import           Ide.Types
import           Language.LSP.Types

pluginsToVSCodeExtensionSchema :: IdePlugins a -> A.Value
pluginsToVSCodeExtensionSchema IdePlugins {..} = A.object $ mconcat $ singlePlugin <$> Map.elems ipMap
  where
    singlePlugin PluginDescriptor {..} = genericConfig <> dedicatedConfig
      where
        (PluginHandlers (DMap.toList -> handlers)) = pluginHandlers
        customConfigToDedicatedConfig (CustomConfig p) = toVSCodeExtensionSchema (withIdPrefix "config.") p
        (PluginId pId) = pluginId
        genericConfig = withIdPrefix "globalOn" A..= methodEntry "plugin" : mconcat (handlersToGenericConfig <$> handlers)
        dedicatedConfig = customConfigToDedicatedConfig pluginCustomConfig
        handlersToGenericConfig (IdeMethod m DSum.:=> _) = case m of
          STextDocumentCodeAction -> [withIdPrefix "codeActionsOn" A..= methodEntry "code actions"]
          STextDocumentCodeLens -> [withIdPrefix "codeLensOn" A..= methodEntry "code lenses"]
          STextDocumentRename -> [withIdPrefix "renameOn" A..= methodEntry "rename"]
          STextDocumentHover -> [withIdPrefix "hoverOn" A..= methodEntry "hover"]
          STextDocumentDocumentSymbol -> [withIdPrefix "symbolsOn" A..= methodEntry "symbols"]
          STextDocumentCompletion -> [withIdPrefix "completionOn" A..= methodEntry "completions"]
          _ -> []
        methodEntry desc =
          A.object
            [ "scope" A..= A.String "resource",
              "type" A..= A.String "boolean",
              "default" A..= True,
              "description" A..= A.String ("Enables " <> pId <> " " <> desc)
            ]
        withIdPrefix x = "haskell.plugin." <> pId <> "." <> x
