{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A plugin that uses tactics to synthesize code
module Wingman.Plugin where

import           Control.Monad
import           Development.IDE.Core.Shake (IdeState (..))
import           Ide.Types
import           Language.LSP.Types
import           Prelude hiding (span)
import           Wingman.AbstractLSP
import           Wingman.AbstractLSP.TacticActions (makeTacticInteraction)
import           Wingman.EmptyCase
import           Wingman.LanguageServer
import           Wingman.LanguageServer.Metaprogram (hoverProvider)
import           Wingman.StaticPlugin


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId
  = installInteractions
      ( emptyCaseInteraction
      : fmap makeTacticInteraction [minBound .. maxBound]
      )
  $ (defaultPluginDescriptor plId)
      { pluginHandlers = mkPluginHandler STextDocumentHover hoverProvider
      , pluginRules = wingmanRules plId
      , pluginConfigDescriptor =
          defaultConfigDescriptor
            { configCustomConfig = mkCustomConfig properties
            }
      , pluginModifyDynflags = staticPlugin
      }

