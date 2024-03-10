{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Ide.Plugin.InlayHints(descriptor) where

import           Control.DeepSeq                      (NFData (rnf), rwhnf)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Data.Either                          (isRight)
import           Data.Hashable                        (Hashable)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe)
import           Data.String                          (IsString (fromString))
import           Data.Text                            (Text)
import           Development.IDE                      (GhcSessionDeps (GhcSessionDeps),
                                                       HieAstResult (HAR, refMap),
                                                       IdeState,
                                                       Position (Position),
                                                       Pretty (pretty),
                                                       RuleResult, Rules,
                                                       TcModuleResult (tmrTypechecked),
                                                       TypeCheck (TypeCheck),
                                                       cmapWithPrio, define,
                                                       hscEnv, printOutputable,
                                                       use_)
import           Development.IDE.Core.PluginUtils     (runActionE,
                                                       useWithStaleE)
import           Development.IDE.Core.PositionMapping (idDelta)
import           Development.IDE.Core.RuleTypes       (GetHieAst (GetHieAst))
import           Development.IDE.Core.Shake           (addPersistentRule)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat           (Fixity (Fixity), Name,
                                                       TcGblEnv, defaultFixity,
                                                       initTcWithGbl,
                                                       lookupFixityRn,
                                                       mkRealSrcLoc,
                                                       realSrcLocSpan,
                                                       realSrcSpanEnd,
                                                       realSrcSpanStart,
                                                       srcLocCol, srcLocLine)
import           Development.IDE.GHC.Compat.Core      (HscEnv)
import qualified Development.IDE.GHC.Compat.Util      as Util
import           GHC.Generics                         (Generic)
import           Ide.Logger                           (Recorder, WithPriority)
import           Ide.Plugin.Error                     (getNormalizedFilePathE)
import           Ide.Types                            (PluginDescriptor (pluginHandlers, pluginRules),
                                                       PluginId,
                                                       PluginMethodHandler,
                                                       defaultPluginDescriptor,
                                                       mkPluginHandler)
import           Language.LSP.Protocol.Message        (Method (Method_TextDocumentInlayHint),
                                                       SMethod (SMethod_TextDocumentInlayHint))
import           Language.LSP.Protocol.Types          (InlayHint (InlayHint),
                                                       InlayHintParams (InlayHintParams),
                                                       TextDocumentIdentifier (TextDocumentIdentifier),
                                                       maybeToNull,
                                                       type (|?) (InL))

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = (defaultPluginDescriptor pluginId "Provides Info in Inlay Hints")
    {
    pluginRules = fixityRule recorder,
    pluginHandlers = mkPluginHandler SMethod_TextDocumentInlayHint inlayHint
    }

inlayHint :: PluginMethodHandler IdeState Method_TextDocumentInlayHint
inlayHint state _pid (InlayHintParams _ (TextDocumentIdentifier uri) _range) = do
    nfp <- getNormalizedFilePathE uri
    runActionE "InlayHints" state $ do
        (FixityMap fixmap, _) <- useWithStaleE GetFixity nfp
        pure $ maybeToNull $ toAbsInlayHints fixmap
    where
        toAbsInlayHints :: M.Map Position Fixity -> Maybe [InlayHint]
        toAbsInlayHints fixmap =
            Just (M.elems $ M.mapWithKey (\(Position x y) (Fixity _ pre direction) ->
              InlayHint
                (Position (x - 1) (y - 1))
                (InL (printOutputable direction <> printOutputable pre))
                Nothing Nothing Nothing Nothing Nothing Nothing
              ) fixmap)

newtype Log = LogShake Shake.Log

instance Pretty Log where
    pretty = \case
        LogShake log -> pretty log

newtype FixityMap = FixityMap (M.Map Position Fixity)
instance Show FixityMap where
  show _ = "FixityMap"

instance NFData FixityMap where
  rnf (FixityMap xs) = rnf xs

instance NFData Fixity where
  rnf = rwhnf

data GetFixity = GetFixity deriving (Show, Eq, Generic)

instance Hashable GetFixity
instance NFData GetFixity

type instance RuleResult GetFixity = FixityMap

fixityRule :: Recorder (WithPriority Log) -> Rules ()
fixityRule recorder = do
    define (cmapWithPrio LogShake recorder) $ \GetFixity nfp -> do
        HAR{refMap} <- use_ GetHieAst nfp
        -- deps necessary so that we can consult already loaded in ifaces instead of loading in duplicates
        env <- hscEnv <$> use_ GhcSessionDeps nfp
        tcGblEnv <- tmrTypechecked <$> use_ TypeCheck nfp
        fs <- lookupFixities env tcGblEnv $
            M.mapKeys (\(Right x) -> x)
            $ M.filterWithKey (\k _ -> isRight k)
            $ M.map
                (fmap (
                (\loc -> Position (fromIntegral $ srcLocLine loc) (fromIntegral $ srcLocCol loc))
                . realSrcSpanEnd
                . fst))
                refMap
        pure ([], Just (FixityMap fs))

    -- Ensure that this plugin doesn't block on startup
    addPersistentRule GetFixity $ const $ pure $ Just (FixityMap M.empty, idDelta, Nothing)

-- | Convert a HieAST to FixityTree with fixity info gathered
lookupFixities :: MonadIO m => HscEnv -> TcGblEnv -> M.Map Name [Position] -> m (M.Map Position Fixity)
lookupFixities hscEnv tcGblEnv names
    = liftIO
    $ fmap (fromMaybe M.empty . snd)
    $ initTcWithGbl hscEnv tcGblEnv (realSrcLocSpan $ mkRealSrcLoc "<dummy>" 0 0)
    $ M.traverseMaybeWithKey (\_ v -> v)
    $ M.fromList
    $ concat
    $ M.elems
    $ M.mapWithKey lookupFixity names
  where
    lookupFixity name positions =
      fmap (,fixity) positions
      where
        fixity = do
          f <- Util.handleGhcException
            (const $ pure Nothing)
            (Just <$> lookupFixityRn name)
          if f == Just defaultFixity
          then pure Nothing
          else pure f
