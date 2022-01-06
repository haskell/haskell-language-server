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
import           Wingman.LanguageServer hiding (Log)
import qualified Wingman.LanguageServer as WingmanLanguageServer
import           Wingman.LanguageServer.Metaprogram (hoverProvider)
import           Wingman.StaticPlugin
import Development.IDE.Types.Logger (Recorder, cmap)

data Log
  = LogLanguageServer WingmanLanguageServer.Log 
  deriving Show

descriptor :: Recorder Log -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId
  = installInteractions
      ( emptyCaseInteraction
      : fmap makeTacticInteraction [minBound .. maxBound]
      )
  $ (defaultPluginDescriptor plId)
      { pluginHandlers = mkPluginHandler STextDocumentHover hoverProvider
      , pluginRules = wingmanRules (cmap LogLanguageServer recorder) plId
      , pluginConfigDescriptor =
          defaultConfigDescriptor
            { configCustomConfig = mkCustomConfig properties
            }
      , pluginModifyDynflags = staticPlugin
      }

