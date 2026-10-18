{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.ExplicitFixity(descriptor, Log) where

import qualified Data.Map.Strict                   as M
import           Data.Maybe
import qualified Data.Text                         as T
import           Development.IDE                   hiding (pluginHandlers,
                                                    pluginRules)
import           Development.IDE.Core.PluginUtils
import qualified Development.IDE.Core.Shake        as Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.LSP.Notifications (ghcideNotificationsPluginPriority)
import           Development.IDE.Spans.AtPoint
import           Ide.Plugin.Error
import           Ide.Types                         hiding (pluginId)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _recorder pluginId = (defaultPluginDescriptor pluginId "Provides fixity information in hovers")
    { pluginHandlers = mkPluginHandler SMethod_TextDocumentHover hover
    -- Make this plugin has a lower priority than ghcide's plugin to ensure
    -- type info display first.
    , pluginPriority = ghcideNotificationsPluginPriority - 1
    }

hover :: PluginMethodHandler IdeState Method_TextDocumentHover
hover state _ (HoverParams (TextDocumentIdentifier uri) pos _) = do
    nfp <- getNormalizedFilePathE uri
    runIdeActionE "ExplicitFixity" (shakeExtras state) $ do
      (FixityMap fixmap, _) <-  useWithStaleFastE GetFixity nfp
      (HAR{hieAst}, mapping) <- useWithStaleFastE GetHieAst nfp
      let ns = getNamesAtPoint hieAst pos mapping
          fs = mapMaybe (\n -> (n,) <$> M.lookup n fixmap) ns
      pure $ maybeToNull $ toHover fs
    where
        toHover :: [(Name, Fixity)] -> Maybe Hover
        toHover [] = Nothing
        toHover fixities =
            let -- Splicing fixity info
                contents = T.intercalate "\n\n" $ fixityText <$> fixities
                -- Append to the previous hover content
                contents' = "\n" <> sectionSeparator <> contents
            in  Just $ Hover (InL (mkPlainText contents')) Nothing

        fixityText :: (Name, Fixity) -> T.Text
#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)
        fixityText (name, Fixity precedence direction) =
            printOutputable direction <> " " <> printOutputable precedence <> " `" <> printOutputable name <> "`"
#else
        fixityText (name, Fixity _ precedence direction) =
            printOutputable direction <> " " <> printOutputable precedence <> " `" <> printOutputable name <> "`"
#endif

newtype Log = LogShake Shake.Log

instance Pretty Log where
    pretty = \case
        LogShake log -> pretty log
