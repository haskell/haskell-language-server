{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -Wall            #-}

module Ide.Plugin.Tactic.LanguageServer where

import           ConLike
import           Control.Arrow
import           Control.Monad
import           Control.Monad.State (State, get, put, evalState)
import           Control.Monad.Trans.Maybe
import           Data.Aeson (Value (Object), fromJSON)
import           Data.Aeson.Types (Result (Error, Success))
import           Data.Coerce
import           Data.Functor ((<&>))
import           Data.Generics.Aliases (mkQ)
import           Data.Generics.Schemes (everything)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set  as S
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE (ShakeExtras, getPluginConfig)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service (runAction)
import           Development.IDE.Core.Shake (IdeState (..), useWithStale)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error (realSrcSpanToRange)
import           Development.IDE.Spans.LocalBindings (Bindings, getDefiningBindings)
import           Development.Shake (Action, RuleResult)
import           Development.Shake.Classes (Typeable, Binary, Hashable, NFData)
import qualified FastString
import           GhcPlugins (mkAppTys, tupleDataCon, consDataCon)
import           Ide.Plugin.Config (PluginConfig (plcConfig))
import qualified Ide.Plugin.Config as Plugin
import           Ide.Plugin.Tactic.Context
import           Ide.Plugin.Tactic.FeatureSet
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Range
import           Ide.Plugin.Tactic.TestTypes (TacticCommand, cfg_feature_set, emptyConfig, Config)
import           Ide.Plugin.Tactic.Types
import           Language.LSP.Server (MonadLsp)
import           Language.LSP.Types
import           OccName
import           Prelude hiding (span)
import           SrcLoc (containsSpan)
import           TcRnTypes (tcg_binds)


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


------------------------------------------------------------------------------
-- | Combine two (possibly-overlapping) hypotheses; using the provenance from
-- the first hypothesis if the bindings overlap.
spliceProvenance
    :: Hypothesis a  -- ^ Bindings to keep
    -> Hypothesis a  -- ^ Bindings to keep if they don't overlap with the first set
    -> Hypothesis a
spliceProvenance top x =
  let bound = S.fromList $ fmap hi_name $ unHypothesis top
   in mappend top $ Hypothesis . filter (flip S.notMember bound . hi_name) $ unHypothesis x


------------------------------------------------------------------------------
-- | Compute top-level position vals of a function
getRhsPosVals :: RealSrcSpan -> TypecheckedSource -> Hypothesis CType
getRhsPosVals rss tcs
  = everything (<>) (mkQ mempty $ \case
      TopLevelRHS name ps
          (L (RealSrcSpan span)  -- body with no guards and a single defn
            (HsVar _ (L _ hole)))
        | containsSpan rss span  -- which contains our span
        , isHole $ occName hole  -- and the span is a hole
        -> flip evalState 0 $ buildTopLevelHypothesis name ps
      _ -> mempty
  ) tcs


------------------------------------------------------------------------------
-- | Construct a hypothesis given the patterns from the left side of a HsMatch.
-- These correspond to things that the user put in scope before running
-- tactics.
buildTopLevelHypothesis
    :: OccName  -- ^ Function name
    -> [PatCompat GhcTc]
    -> State Int (Hypothesis CType)
buildTopLevelHypothesis name ps = do
  fmap mconcat $
    for (zip [0..] ps) $ \(ix, p) ->
      buildPatHy (TopLevelArgPrv name ix $ length ps) p


------------------------------------------------------------------------------
-- | Construct a hypothesis for a single pattern, including building
-- sub-hypotheses for constructor pattern matches.
buildPatHy :: Provenance -> PatCompat GhcTc -> State Int (Hypothesis CType)
buildPatHy prov (fromPatCompatTc -> p0) =
  case p0 of
    VarPat  _ x   -> pure $ mkIdHypothesis (unLoc x) prov
    LazyPat _ p   -> buildPatHy prov p
    AsPat   _ x p -> do
      hy' <- buildPatHy prov p
      pure $ mkIdHypothesis (unLoc x) prov <> hy'
    ParPat  _ p   -> buildPatHy prov p
    BangPat _ p   -> buildPatHy prov p
    ViewPat _ _ p -> buildPatHy prov p
    -- Desugar lists into cons
    ListPat _ [] -> pure mempty
    ListPat x@(ListPatTc ty _) (p : ps) ->
      mkDerivedConHypothesis prov consDataCon [ty]
        [ (0, p)
        , (1, toPatCompatTc $ ListPat x ps)
        ]
    -- Desugar tuples into an explicit constructor
    TuplePat tys pats boxity ->
      mkDerivedConHypothesis
        prov
        (tupleDataCon boxity $ length pats)
        tys
          $ zip [0.. ] pats
    ConPatOut (L _ (RealDataCon dc)) args _ _ _ f _ ->
      case f of
        PrefixCon l_pgt ->
          mkDerivedConHypothesis prov dc args $ zip [0..] l_pgt
        InfixCon pgt pgt5 ->
          mkDerivedConHypothesis prov dc args $ zip [0..] [pgt, pgt5]
        RecCon r ->
          mkDerivedRecordHypothesis prov dc args r
#if __GLASGOW_HASKELL__ >= 808
    SigPat  _ p _ -> buildPatHy prov p
#endif
#if __GLASGOW_HASKELL__ == 808
    XPat   p      -> buildPatHy prov $ unLoc p
#endif
    _             -> pure mempty


------------------------------------------------------------------------------
-- | Like 'mkDerivedConHypothesis', but for record patterns.
mkDerivedRecordHypothesis
    :: Provenance
    -> DataCon  -- ^ Destructing constructor
    -> [Type]   -- ^ Applied type variables
    -> HsRecFields GhcTc (PatCompat GhcTc)
    -> State Int (Hypothesis CType)
mkDerivedRecordHypothesis prov dc args (HsRecFields (fmap unLoc -> fs) _)
  | Just rec_fields <- getRecordFields dc
  = do
    let field_lookup = M.fromList $ zip (fmap (occNameFS . fst) rec_fields) [0..]
    mkDerivedConHypothesis prov dc args $ fs <&> \(HsRecField (L _ rec_occ) p _) ->
      ( field_lookup M.! (occNameFS $ occName $ unLoc $ rdrNameFieldOcc rec_occ)
      , p
      )
mkDerivedRecordHypothesis _ _ _ _ =
  error "impossible! using record pattern on something that isn't a record"


------------------------------------------------------------------------------
-- | Construct a fake variable name. Used to track the provenance of top-level
-- pattern matches which otherwise wouldn't have anything to attach their
-- 'TopLevelArgPrv' to.
mkFakeVar :: State Int OccName
mkFakeVar = do
  i <- get
  put $ i + 1
  pure $ mkVarOcc $ "_" <> show i


------------------------------------------------------------------------------
-- | Construct a fake varible to attach the current 'Provenance' to, and then
-- build a sub-hypothesis for the pattern match.
mkDerivedConHypothesis
    :: Provenance
    -> DataCon                   -- ^ Destructing constructor
    -> [Type]                    -- ^ Applied type variables
    -> [(Int, PatCompat GhcTc)]  -- ^ Patterns, and their order in the data con
    -> State Int (Hypothesis CType)
mkDerivedConHypothesis prov dc args ps = do
  var <- mkFakeVar
  hy' <- fmap mconcat $
    for ps $ \(ix, p) -> do
      let prov' = PatternMatchPrv
               $ PatVal (Just var)
                        (S.singleton var <> provAncestryOf prov)
                        (Uniquely dc)
                        ix
      buildPatHy prov' p
  pure
    $ mappend hy'
    $ Hypothesis
    $ pure
    $ HyInfo var (DisallowedPrv AlreadyDestructed prov)
    $ CType
    -- TODO(sandy): This is the completely wrong type, but we don't have a good
    -- way to get the real one. It's probably OK though, since we're generating
    -- this term with a disallowed provenance, and it doesn't actually exist
    -- anyway.
    $ mkAppTys (dataConUserType dc) args


------------------------------------------------------------------------------
-- | Build a 'Hypothesis' given an 'Id'.
mkIdHypothesis :: Id -> Provenance -> Hypothesis CType
mkIdHypothesis (splitId -> (name, ty)) prov =
  Hypothesis $ pure $ HyInfo name prov ty


------------------------------------------------------------------------------
-- | Is this hole immediately to the right of an equals sign?
isRhsHole :: RealSrcSpan -> TypecheckedSource -> Bool
isRhsHole rss tcs = everything (||) (mkQ False $ \case
  TopLevelRHS _ _ (L (RealSrcSpan span) _) -> containsSpan rss span
  _                                        -> False
  ) tcs

