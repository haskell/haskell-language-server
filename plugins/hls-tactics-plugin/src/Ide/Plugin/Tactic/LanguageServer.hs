{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin.Tactic.LanguageServer where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Aeson                           (Value (Object), fromJSON)
import           Data.Aeson.Types                     (Result (Error, Success))
import           Data.Coerce
import           Data.Functor                         ((<&>))
import           Data.Generics.Aliases                (mkQ)
import           Data.Generics.Schemes                (everything)
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set                             as S
import qualified Data.Text                            as T
import           Data.Traversable
import           Development.IDE                      (ShakeExtras,
                                                       getPluginConfig)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service         (runAction)
import           Development.IDE.Core.Shake           (IdeState (..),
                                                       useWithStale)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error            (realSrcSpanToRange)
import           Development.IDE.Spans.LocalBindings  (Bindings,
                                                       getDefiningBindings)
import           Development.Shake                    (Action, RuleResult)
import           Development.Shake.Classes
import qualified FastString
import           Ide.Plugin.Config                    (PluginConfig (plcConfig))
import qualified Ide.Plugin.Config                    as Plugin
import           Ide.Plugin.Tactic.Context
import           Ide.Plugin.Tactic.FeatureSet
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Range
import           Ide.Plugin.Tactic.TestTypes          (Config, TacticCommand,
                                                       cfg_feature_set,
                                                       emptyConfig)
import           Ide.Plugin.Tactic.Types
import           Language.LSP.Server                  (MonadLsp)
import           Language.LSP.Types
import           OccName
import           Prelude                              hiding (span)
import           SrcLoc                               (containsSpan)
import           TcRnTypes                            (tcg_binds)


tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"


------------------------------------------------------------------------------
-- | The name of the command for the LS.
tcCommandName :: TacticCommand -> T.Text
tcCommandName = T.pack . show


runIde :: IdeState -> Action a -> IO a
runIde state = runAction "tactic" state


runStaleIde
    :: forall a r
     . ( r ~ RuleResult a
       , Eq a , Hashable a , Binary a , Show a , Typeable a , NFData a
       , Show r, Typeable r, NFData r
       )
    => IdeState
    -> NormalizedFilePath
    -> a
    -> MaybeT IO (r, PositionMapping)
runStaleIde state nfp a = MaybeT $ runIde state $ useWithStale a nfp


------------------------------------------------------------------------------
-- | Get the the plugin config
getTacticConfig :: MonadLsp Plugin.Config m => ShakeExtras -> m Config
getTacticConfig extras = do
  pcfg <- getPluginConfig extras "tactics"
  pure $ case fromJSON $ Object $ plcConfig pcfg of
    Success cfg -> cfg
    Error _     -> emptyConfig


------------------------------------------------------------------------------
-- | Get the current feature set from the plugin config.
getFeatureSet :: MonadLsp Plugin.Config m => ShakeExtras -> m FeatureSet
getFeatureSet  = fmap cfg_feature_set . getTacticConfig


getIdeDynflags
    :: IdeState
    -> NormalizedFilePath
    -> MaybeT IO DynFlags
getIdeDynflags state nfp = do
  -- Ok to use the stale 'ModIface', since all we need is its 'DynFlags'
  -- which don't change very often.
  (msr, _) <- runStaleIde state nfp GetModSummaryWithoutTimestamps
  pure $ ms_hspp_opts $ msrModSummary msr


------------------------------------------------------------------------------
-- | Find the last typechecked module, and find the most specific span, as well
-- as the judgement at the given range.
judgementForHole
    :: IdeState
    -> NormalizedFilePath
    -> Range
    -> FeatureSet
    -> MaybeT IO (Range, Judgement, Context, DynFlags)
judgementForHole state nfp range features = do
  (asts, amapping) <- runStaleIde state nfp GetHieAst
  case asts of
    HAR _ _  _ _ (HieFromDisk _) -> fail "Need a fresh hie file"
    HAR _ hf _ _ HieFresh -> do
      (binds, _) <- runStaleIde state nfp GetBindings
      (tcmod, _) <- runStaleIde state nfp TypeCheck
      (rss, g)   <- liftMaybe $ getSpanAndTypeAtHole amapping range hf
      resulting_range <- liftMaybe $ toCurrentRange amapping $ realSrcSpanToRange rss
      let (jdg, ctx) = mkJudgementAndContext features g binds rss tcmod
      dflags <- getIdeDynflags state nfp
      pure (resulting_range, jdg, ctx, dflags)


mkJudgementAndContext
    :: FeatureSet
    -> Type
    -> Bindings
    -> RealSrcSpan
    -> TcModuleResult
    -> (Judgement, Context)
mkJudgementAndContext features g binds rss tcmod = do
      let tcg  = tmrTypechecked tcmod
          tcs = tcg_binds tcg
          ctx = mkContext features
                  (mapMaybe (sequenceA . (occName *** coerce))
                    $ getDefiningBindings binds rss)
                  tcg
          top_provs = getRhsPosVals rss tcs
          local_hy = spliceProvenance top_provs
                   $ hypothesisFromBindings rss binds
          cls_hy = contextMethodHypothesis ctx
       in ( mkFirstJudgement
              (local_hy <> cls_hy)
              (isRhsHole rss tcs)
              g
          , ctx
          )


getSpanAndTypeAtHole
    :: PositionMapping
    -> Range
    -> HieASTs b
    -> Maybe (Span, b)
getSpanAndTypeAtHole amapping range hf = do
  range' <- fromCurrentRange amapping range
  join $ listToMaybe $ M.elems $ flip M.mapWithKey (getAsts hf) $ \fs ast ->
    case selectSmallestContaining (rangeToRealSrcSpan (FastString.unpackFS fs) range') ast of
      Nothing -> Nothing
      Just ast' -> do
        let info = nodeInfo ast'
        ty <- listToMaybe $ nodeType info
        guard $ ("HsUnboundVar","HsExpr") `S.member` nodeAnnotations info
        pure (nodeSpan ast', ty)


liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a


spliceProvenance
    :: Map OccName Provenance
    -> Hypothesis a
    -> Hypothesis a
spliceProvenance provs x =
  Hypothesis $ flip fmap (unHypothesis x) $ \hi ->
    overProvenance (maybe id const $ M.lookup (hi_name hi) provs) hi


------------------------------------------------------------------------------
-- | Compute top-level position vals of a function
getRhsPosVals :: RealSrcSpan -> TypecheckedSource -> Map OccName Provenance
getRhsPosVals rss tcs
  = M.fromList
  $ join
  $ maybeToList
  $ getFirst
  $ everything (<>) (mkQ mempty $ \case
      TopLevelRHS name ps
          (L (RealSrcSpan span)  -- body with no guards and a single defn
            (HsVar _ (L _ hole)))
        | containsSpan rss span  -- which contains our span
        , isHole $ occName hole  -- and the span is a hole
        -> First $ do
            patnames <- traverse getPatName ps
            pure $ zip patnames $ [0..] <&> \n ->
              TopLevelArgPrv name n (length patnames)
      _ -> mempty
  ) tcs


------------------------------------------------------------------------------
-- | Is this hole immediately to the right of an equals sign?
isRhsHole :: RealSrcSpan -> TypecheckedSource -> Bool
isRhsHole rss tcs = everything (||) (mkQ False $ \case
  TopLevelRHS _ _ (L (RealSrcSpan span) _) -> containsSpan rss span
  _                                        -> False
  ) tcs

