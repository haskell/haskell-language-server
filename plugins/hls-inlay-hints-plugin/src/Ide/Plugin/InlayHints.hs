{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.InlayHints(descriptor) where

import           Control.Monad.Cont                 (MonadIO (liftIO))
import           Data.Foldable                      (traverse_)
import           Development.IDE                    (IdeState, runAction)
import           Development.IDE.Core.PluginUtils   (runActionE)
import           Ide.Logger                         (Recorder, WithPriority)
import           Ide.Plugin.Error                   (getNormalizedFilePathE)
import           Ide.Plugin.InlayHints.Config       (InlayHintsConfig (..),
                                                     getInlayHintsConfig,
                                                     properties)
import           Ide.Plugin.InlayHints.Fixity       (fixityInlayHints,
                                                     fixityRule)
import           Ide.Plugin.InlayHints.Hole         (holeInlayHints, holeRule)
import           Ide.Plugin.InlayHints.LocalBinding (localBindingInlayHints,
                                                     localBindingRule)
import           Ide.Plugin.InlayHints.Types        (InlayHintLog)
import           Ide.Types                          (ConfigDescriptor (configCustomConfig),
                                                     PluginDescriptor (pluginConfigDescriptor, pluginHandlers, pluginRules),
                                                     PluginId,
                                                     defaultConfigDescriptor,
                                                     defaultPluginDescriptor,
                                                     mkCustomConfig,
                                                     mkPluginHandler)
import           Language.LSP.Protocol.Message      (SMethod (SMethod_TextDocumentInlayHint))
import           Language.LSP.Protocol.Types        (InlayHintParams (InlayHintParams),
                                                     Null (Null),
                                                     TextDocumentIdentifier (TextDocumentIdentifier),
                                                     type (|?) (InL, InR))

descriptor :: Recorder (WithPriority InlayHintLog) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = (defaultPluginDescriptor pluginId "Provides Info in Inlay Hints")
    { pluginRules = traverse_ ($ recorder) [
        fixityRule
        , holeRule
        , localBindingRule
        ]
    , pluginHandlers =
        mkPluginHandler SMethod_TextDocumentInlayHint
        $ \state _pid (InlayHintParams _ (TextDocumentIdentifier uri) _) -> do
            nfp <- getNormalizedFilePathE uri
            runActionE "InlayHints" state $ do
                inlayHintsCfg <- liftIO $ runAction "inlay hints: config" state $ getInlayHintsConfig pluginId
                let optional p x = if any ($ inlayHintsCfg) [p, enableAll]
                                   then x
                                   else const $ pure (InL [])

                fmap (foldr (<>) (InR Null)) $ traverse (($ nfp) . uncurry optional) [
                    (enableFixity, fixityInlayHints)
                    , (enableHole, holeInlayHints)
                    , (enableLocalBinding, localBindingInlayHints)
                    ]
    , pluginConfigDescriptor = defaultConfigDescriptor
      { configCustomConfig = mkCustomConfig properties
      }
    }
