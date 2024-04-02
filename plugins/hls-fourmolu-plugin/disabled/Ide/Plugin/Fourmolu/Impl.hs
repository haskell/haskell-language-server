{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Fourmolu.Impl (pluginDesc, handlers) where

import           Data.Text                  (Text)
import           Development.IDE            hiding (pluginHandlers)
import           Ide.Plugin.Fourmolu.Common
import           Ide.Types

pluginDesc :: Text
pluginDesc = "DISABLED: " <> pluginDescMain

handlers :: Recorder (WithPriority LogEvent) -> PluginId -> PluginHandlers IdeState
handlers = mempty
