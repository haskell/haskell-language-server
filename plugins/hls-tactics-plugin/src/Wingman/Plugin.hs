-- | A plugin that uses tactics to synthesize code
module Wingman.Plugin where

import           Control.Monad
import           Development.IDE.Core.Shake (IdeState (..))
import           Development.IDE.Plugin.CodeAction
import qualified Development.IDE.GHC.ExactPrint                   as E
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
import Development.IDE.Types.Logger (Recorder, cmapWithPrio, WithPriority, Pretty (pretty))

data Log
  = LogWingmanLanguageServer WingmanLanguageServer.Log 
  | LogExactPrint E.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogWingmanLanguageServer log -> pretty log
    LogExactPrint exactPrintLog -> pretty exactPrintLog

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId
  = mkExactprintPluginDescriptor (cmapWithPrio LogExactPrint recorder)
  $ installInteractions
      ( emptyCaseInteraction
      : fmap makeTacticInteraction [minBound .. maxBound]
      )
  $ (defaultPluginDescriptor plId)
      { pluginHandlers = mkPluginHandler STextDocumentHover hoverProvider
      , pluginRules = wingmanRules (cmapWithPrio LogWingmanLanguageServer recorder) plId
      , pluginConfigDescriptor =
          defaultConfigDescriptor
            { configCustomConfig = mkCustomConfig properties
            }
      , pluginModifyDynflags = staticPlugin
      }

