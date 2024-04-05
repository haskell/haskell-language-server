{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.InlayHints(descriptor) where

import           Development.IDE                    (IdeState)
import           Development.IDE.Core.PluginUtils   (runActionE)
import           Ide.Logger                         (Recorder, WithPriority)
import           Ide.Plugin.Error                   (getNormalizedFilePathE)
import           Ide.Plugin.InlayHints.Fixity       (fixityInlayHints,
                                                     fixityRule)
import           Ide.Plugin.InlayHints.LocalBinding (localBindingInlayHints,
                                                     localBindingRule)
import           Ide.Plugin.InlayHints.Types        (InlayHintLog)
import           Ide.Types                          (PluginDescriptor (pluginHandlers, pluginRules),
                                                     PluginId,
                                                     defaultPluginDescriptor,
                                                     mkPluginHandler)
import           Language.LSP.Protocol.Message      (SMethod (SMethod_TextDocumentInlayHint))
import           Language.LSP.Protocol.Types        (InlayHintParams (InlayHintParams),
                                                     Null (Null),
                                                     TextDocumentIdentifier (TextDocumentIdentifier),
                                                     type (|?) (InR))

descriptor :: Recorder (WithPriority InlayHintLog) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = (defaultPluginDescriptor pluginId "Provides Info in Inlay Hints")
    { pluginRules = do
        fixityRule recorder
        localBindingRule recorder
    , pluginHandlers =
        mkPluginHandler SMethod_TextDocumentInlayHint
        $ \state _pid (InlayHintParams _ (TextDocumentIdentifier uri) _) -> do
            nfp <- getNormalizedFilePathE uri
            runActionE "InlayHints" state $ do
                fmap (foldr (<>) (InR Null)) $ traverse ($ nfp)
                    [
                    fixityInlayHints
                    , localBindingInlayHints
                    ]
    }
