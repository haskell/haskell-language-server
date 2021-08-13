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
import           Wingman.AbstractLSP.TacticActions (makeTacticCodeAction)
import           Wingman.EmptyCase
import           Wingman.LanguageServer
import           Wingman.LanguageServer.Metaprogram (hoverProvider)
import           Wingman.StaticPlugin


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId
  = installInteractions
      ( fmap makeTacticCodeAction [minBound .. maxBound]
      )
  $ (defaultPluginDescriptor plId)
      { pluginCommands =
          mconcat
            [ pure $
                PluginCommand
                emptyCaseLensCommandId
                "Complete the empty case"
                workspaceEditHandler
            ]
      , pluginHandlers = mconcat
          [ mkPluginHandler STextDocumentCodeLens codeLensProvider
          , mkPluginHandler STextDocumentHover hoverProvider
          ]
      , pluginRules = wingmanRules plId
      , pluginConfigDescriptor =
          defaultConfigDescriptor
            { configCustomConfig = mkCustomConfig properties
            }
      , pluginModifyDynflags = staticPlugin
      }

