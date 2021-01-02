{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exposes the ghcide features as an HLS plugin
module Development.IDE.Plugin.HLS.Completions
  (
    descriptor
  ) where

import Data.Aeson
import Development.IDE
import Development.IDE.Plugin as Ghcide
import Development.IDE.Plugin.Completions as Completions
import Ide.PluginUtils
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginCompletionProvider = Just getCompletionsLSP
  , pluginRules              = Ghcide.pluginRules Completions.plugin
  }
