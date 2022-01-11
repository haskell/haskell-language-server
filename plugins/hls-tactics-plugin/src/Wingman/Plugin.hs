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
import Prettyprinter (Pretty (pretty))
import qualified Development.IDE.Types.Logger as Logger

newtype Log
  = LogWingmanLanguageServer WingmanLanguageServer.Log 
  deriving Show

instance Pretty Log where
  pretty = \case
    LogWingmanLanguageServer log -> pretty log
  
logToPriority :: Log -> Logger.Priority
logToPriority = \case 
  LogWingmanLanguageServer log -> WingmanLanguageServer.logToPriority log

descriptor :: Recorder Log -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId
  = installInteractions
      ( emptyCaseInteraction
      : fmap makeTacticInteraction [minBound .. maxBound]
      )
  $ (defaultPluginDescriptor plId)
      { pluginHandlers = mkPluginHandler STextDocumentHover hoverProvider
      , pluginRules = wingmanRules (cmap LogWingmanLanguageServer recorder) plId
      , pluginConfigDescriptor =
          defaultConfigDescriptor
            { configCustomConfig = mkCustomConfig properties
            }
      , pluginModifyDynflags = staticPlugin
      }

