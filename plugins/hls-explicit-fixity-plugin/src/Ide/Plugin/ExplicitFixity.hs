{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Ide.Plugin.ExplicitFixity(descriptor, Log) where

import           Control.DeepSeq
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Data.Either.Extra
import           Data.Hashable
import qualified Data.Map.Strict                      as M
import           Data.Maybe
import qualified Data.Set                             as S
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers,
                                                       pluginRules)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (idDelta)
import           Development.IDE.Core.Shake           (addPersistentRule)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util      as Util
import           Development.IDE.LSP.Notifications    (ghcideNotificationsPluginPriority)
import           Development.IDE.Spans.AtPoint
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Error
import           Ide.Types                            hiding (pluginId)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = (defaultPluginDescriptor pluginId "Provides fixity information in hovers")
    { pluginRules = fixityRule recorder
    , pluginHandlers = mkPluginHandler SMethod_TextDocumentHover hover
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
#else
        fixityText (name, Fixity _ precedence direction) =
#endif
            printOutputable direction <> " " <> printOutputable precedence <> " `" <> printOutputable name <> "`"

newtype FixityMap = FixityMap (M.Map Name Fixity)
instance Show FixityMap where
  show _ = "FixityMap"

instance NFData FixityMap where
  rnf (FixityMap xs) = rnf xs

instance NFData Fixity where
  rnf = rwhnf

newtype Log = LogShake Shake.Log

instance Pretty Log where
    pretty = \case
        LogShake log -> pretty log

data GetFixity = GetFixity deriving (Show, Eq, Generic)

instance Hashable GetFixity
instance NFData GetFixity

type instance RuleResult GetFixity = FixityMap

-- | Convert a HieAST to FixityTree with fixity info gathered
lookupFixities :: MonadIO m => HscEnv -> TcGblEnv -> S.Set Name -> m (M.Map Name Fixity)
lookupFixities hscEnv tcGblEnv names
    = liftIO
    $ fmap (fromMaybe M.empty . snd)
    $ initTcWithGbl hscEnv tcGblEnv (realSrcLocSpan $ mkRealSrcLoc "<dummy>" 1 1)
    $ M.traverseMaybeWithKey (\_ v -> v)
    $ M.fromSet lookupFixity names
  where
    lookupFixity name = do
      f <- Util.handleGhcException
        (const $ pure Nothing)
        (Just <$> lookupFixityRn name)
      if f == Just defaultFixity
      then pure Nothing
      else pure f

fixityRule :: Recorder (WithPriority Log) -> Rules ()
fixityRule recorder = do
    define (cmapWithPrio LogShake recorder) $ \GetFixity nfp -> do
        HAR{refMap} <- use_ GetHieAst nfp
        env <- hscEnv <$> use_ GhcSessionDeps nfp -- deps necessary so that we can consult already loaded in ifaces instead of loading in duplicates
        tcGblEnv <- tmrTypechecked <$> use_ TypeCheck nfp
        fs <- lookupFixities env tcGblEnv (S.mapMonotonic (\(Right n) -> n) $ S.filter isRight $ M.keysSet refMap)
        pure ([], Just (FixityMap fs))

    -- Ensure that this plugin doesn't block on startup
    addPersistentRule GetFixity $ \_ -> pure $ Just (FixityMap M.empty, idDelta, Nothing)
