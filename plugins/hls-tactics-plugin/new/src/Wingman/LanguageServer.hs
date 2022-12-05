{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# LANGUAGE NoMonoLocalBinds  #-}

module Wingman.LanguageServer where

import           Control.Arrow ((&&&))
import           Control.Monad
import           Control.Monad.RWS
import           Control.Monad.State (State, evalState)
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor (first)
import           Data.Functor ((<&>))
import qualified Data.HashMap.Strict as Map
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE (getFilesOfInterestUntracked, ShowDiagnostic (ShowDiag), srcSpanToRange, IdeAction)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Rules (usePropertyAction)
import           Development.IDE.Core.Service (runAction)
import           Development.IDE.Core.Shake (IdeState (..), uses, define, use)
import qualified Development.IDE.Core.Shake as IDE
import qualified Development.IDE.Core.Shake as Shake
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat hiding (empty)
import           Development.IDE.GHC.Compat.ExactPrint
import qualified Development.IDE.GHC.Compat.Util as FastString
import           Development.IDE.GHC.Error (realSrcSpanToRange)
import           Development.IDE.GHC.ExactPrint hiding (LogShake, Log)
import           Development.IDE.Graph (Action, RuleResult, Rules, action)
import           Development.IDE.Graph.Classes (Hashable, NFData)
import           Development.IDE.Spans.LocalBindings (Bindings)
import           Development.IDE.Types.Logger (Recorder, cmapWithPrio, WithPriority, Pretty (pretty))
import           GHC.Generics (Generic)
import           Generics.SYB hiding (Generic)
import qualified Ide.Plugin.Config as Plugin
import           Ide.Plugin.Properties
import           Ide.PluginUtils (usePropertyLsp)
import           Ide.Types (PluginId)
import           Language.LSP.Server (MonadLsp, sendNotification)
import           Language.LSP.Types hiding (SemanticTokenAbsolute (length, line), SemanticTokenRelative (length), SemanticTokensEdit (_start))
import           Language.LSP.Types.Capabilities
import           Prelude hiding (span)
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Judgements.SYB (everythingContaining)
import           Wingman.Range
import           Wingman.Types


newtype Log
  = LogShake Shake.Log
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog

tacticDesc :: T.Text -> T.Text
tacticDesc name = "fill the hole using the " <> name <> " tactic"


------------------------------------------------------------------------------
-- | The name of the command for the LS.
tcCommandName :: TacticCommand -> T.Text
tcCommandName = T.pack . show


runIde :: String -> String -> IdeState -> Action a -> IO a
runIde herald action state = runAction ("Wingman." <> herald <> "." <> action) state

runIdeAction :: String -> String -> IdeState -> IdeAction a -> IO a
runIdeAction herald action state = IDE.runIdeAction ("Wingman." <> herald <> "." <> action) (shakeExtras state)


runCurrentIde
    :: forall a r
     . ( r ~ RuleResult a
       , Eq a , Hashable a , Show a , Typeable a , NFData a
       , Show r, Typeable r, NFData r
       )
    => String
    -> IdeState
    -> NormalizedFilePath
    -> a
    -> MaybeT IO (Tracked 'Current r)
runCurrentIde herald state nfp a =
  MaybeT $ fmap (fmap unsafeMkCurrent) $ runIde herald (show a) state $ use a nfp


runStaleIde
    :: forall a r
     . ( r ~ RuleResult a
       , Eq a , Hashable a , Show a , Typeable a , NFData a
       , Show r, Typeable r, NFData r
       )
    => String
    -> IdeState
    -> NormalizedFilePath
    -> a
    -> MaybeT IO (TrackedStale r)
runStaleIde herald state nfp a =
  MaybeT $ runIde herald (show a) state $ useWithStale a nfp


unsafeRunStaleIde
    :: forall a r
     . ( r ~ RuleResult a
       , Eq a , Hashable a , Show a , Typeable a , NFData a
       , Show r, Typeable r, NFData r
       )
    => String
    -> IdeState
    -> NormalizedFilePath
    -> a
    -> MaybeT IO r
unsafeRunStaleIde herald state nfp a = do
  (r, _) <- MaybeT $ runIde herald (show a) state $ IDE.useWithStale a nfp
  pure r

unsafeRunStaleIdeFast
    :: forall a r
     . ( r ~ RuleResult a
       , Eq a , Hashable a , Show a , Typeable a , NFData a
       , Show r, Typeable r, NFData r
       )
    => String
    -> IdeState
    -> NormalizedFilePath
    -> a
    -> MaybeT IO r
unsafeRunStaleIdeFast herald state nfp a = do
  (r, _) <- MaybeT $ runIdeAction herald (show a) state $ IDE.useWithStaleFast a nfp
  pure r


------------------------------------------------------------------------------

properties :: Properties
  '[ 'PropertyKey "hole_severity" ('TEnum (Maybe DiagnosticSeverity))
   , 'PropertyKey "max_use_ctor_actions" 'TInteger
   , 'PropertyKey "timeout_duration" 'TInteger
   , 'PropertyKey "auto_gas" 'TInteger
   , 'PropertyKey "proofstate_styling" 'TBoolean
   ]
properties = emptyProperties
  & defineBooleanProperty #proofstate_styling
    "Should Wingman emit styling markup when showing metaprogram proof states?" True
  & defineIntegerProperty #auto_gas
    "The depth of the search tree when performing \"Attempt to fill hole\". Bigger values will be able to derive more solutions, but will take exponentially more time." 4
  & defineIntegerProperty #timeout_duration
    "The timeout for Wingman actions, in seconds" 2
  & defineIntegerProperty #max_use_ctor_actions
    "Maximum number of `Use constructor <x>` code actions that can appear" 5
  & defineEnumProperty #hole_severity
    "The severity to use when showing hole diagnostics. These are noisy, but some editors don't allow jumping to all severities."
    [ (Just DsError,   "error")
    , (Just DsWarning, "warning")
    , (Just DsInfo,    "info")
    , (Just DsHint,    "hint")
    , (Nothing,        "none")
    ]
    Nothing


-- | Get the the plugin config
getTacticConfig :: MonadLsp Plugin.Config m => PluginId -> m Config
getTacticConfig pId =
  Config
    <$> usePropertyLsp #max_use_ctor_actions pId properties
    <*> usePropertyLsp #timeout_duration pId properties


getIdeDynflags
    :: IdeState
    -> NormalizedFilePath
    -> MaybeT IO DynFlags
getIdeDynflags state nfp = do
  -- Ok to use the stale 'ModIface', since all we need is its 'DynFlags'
  -- which don't change very often.
  msr <- unsafeRunStaleIde "getIdeDynflags" state nfp GetModSummaryWithoutTimestamps
  pure $ ms_hspp_opts $ msrModSummary msr


------------------------------------------------------------------------------
-- | Find the last typechecked module, and find the most specific span, as well
-- as the judgement at the given range.
judgementForHole
    :: IdeState
    -> NormalizedFilePath
    -> Tracked 'Current Range
    -> Config
    -> MaybeT IO HoleJudgment
judgementForHole state nfp range cfg = do
  let stale a = runStaleIde "judgementForHole" state nfp a

  TrackedStale asts amapping  <- stale GetHieAst
  case unTrack asts of
    HAR _ _  _ _ (HieFromDisk _) -> fail "Need a fresh hie file"
    HAR _ (unsafeCopyAge asts -> hf) _ _ HieFresh -> do
      range' <- liftMaybe $ mapAgeFrom amapping range
      binds <- stale GetBindings
      tcg <- fmap (fmap tmrTypechecked) $ stale TypeCheck

      (rss, g) <- liftMaybe $ getSpanAndTypeAtHole range' hf

      new_rss <- liftMaybe $ mapAgeTo amapping rss

      (jdg, ctx) <- liftMaybe $ mkJudgementAndContext cfg g binds new_rss tcg

      dflags <- getIdeDynflags state nfp
      pure $ HoleJudgment
        { hj_range = fmap realSrcSpanToRange new_rss
        , hj_jdg = jdg
        , hj_ctx = ctx
        , hj_dflags = dflags
        }


mkJudgementAndContext
    :: Config
    -> Type
    -> TrackedStale Bindings
    -> Tracked 'Current RealSrcSpan
    -> TrackedStale TcGblEnv
    -> Maybe (Judgement, Config)
mkJudgementAndContext cfg g (TrackedStale binds bmap) rss (TrackedStale tcg tcgmap) = do
  binds_rss <- mapAgeFrom bmap rss
  tcg_rss <- mapAgeFrom tcgmap rss

  let tcs = fmap tcg_binds tcg
      top_provs = getRhsPosVals tcg_rss tcs
      already_destructed = getAlreadyDestructed (fmap (`RealSrcSpan` Nothing) tcg_rss) tcs
      local_hy = spliceProvenance top_provs
               $ hypothesisFromBindings binds_rss binds
      subst = ts_unifier defaultTacticState
  pure
    ( disallowing AlreadyDestructed already_destructed
    $ fmap (CType . substTyAddInScope subst . unCType) $
        mkFirstJudgement
          (local_hy)
          (isRhsHoleWithoutWhere tcg_rss tcs)
          g
    , cfg
    )


------------------------------------------------------------------------------
-- | Determine which bindings have already been destructed by the location of
-- the hole.
getAlreadyDestructed
    :: Tracked age SrcSpan
    -> Tracked age (LHsBinds GhcTc)
    -> Set OccName
getAlreadyDestructed (unTrack -> span) (unTrack -> binds) =
  everythingContaining span
    (mkQ mempty $ \case
      Case (HsVar _ (L _ (occName -> var))) _ ->
        S.singleton var
      (_ :: HsExpr GhcTc) -> mempty
    ) binds


getSpanAndTypeAtHole
    :: Tracked age Range
    -> Tracked age (HieASTs Type)
    -> Maybe (Tracked age RealSrcSpan, Type)
getSpanAndTypeAtHole r@(unTrack -> range) (unTrack -> hf) = do
  join $ listToMaybe $ M.elems $ flip M.mapWithKey (getAsts hf) $ \fs ast ->
    case selectSmallestContaining (rangeToRealSrcSpan (FastString.unpackFS fs) range) ast of
      Nothing -> Nothing
      Just ast' -> do
        let info = nodeInfo ast'
        ty <- listToMaybe $ nodeType info
        guard $ ("HsUnboundVar","HsExpr") `S.member` nodeAnnotations info
        -- Ensure we're actually looking at a hole here
        occ <- (either (const Nothing) (Just . occName) =<<)
             . listToMaybe
             . S.toList
             . M.keysSet
             $ nodeIdentifiers info
        guard $ isHole occ
        pure (unsafeCopyAge r $ nodeSpan ast', ty)



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
getRhsPosVals
    :: Tracked age RealSrcSpan
    -> Tracked age TypecheckedSource
    -> Hypothesis CType
getRhsPosVals (unTrack -> rss) (unTrack -> tcs)
  = everything (<>) (mkQ mempty $ \case
      TopLevelRHS name ps
          (L (RealSrcSpan span _)  -- body with no guards and a single defn
            (HsVar _ (L _ hole)))
          _
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
buildPatHy prov (fromPatCompat -> p0) =
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
      mkDerivedConHypothesis prov (RealDataCon consDataCon) [ty]
        [ (0, p)
        , (1, toPatCompat $ ListPat x ps)
        ]
    -- Desugar tuples into an explicit constructor
    TuplePat tys pats boxity ->
      mkDerivedConHypothesis
        prov
        (RealDataCon $ tupleDataCon boxity $ length pats)
        tys
          $ zip [0.. ] pats
#if __GLASGOW_HASKELL__ >= 900
    ConPat {pat_con = (L _ con), pat_con_ext = ConPatTc {cpt_arg_tys = args}, pat_args = f} ->
#else
    ConPatOut {pat_con = (L _ con), pat_arg_tys = args, pat_args = f} ->
#endif
      case f of
        PrefixCon l_pgt ->
          mkDerivedConHypothesis prov con args $ zip [0..] l_pgt
        InfixCon pgt pgt5 ->
          mkDerivedConHypothesis prov con args $ zip [0..] [pgt, pgt5]
        RecCon r ->
          mkDerivedRecordHypothesis prov con args r
    SigPat  _ p _ -> buildPatHy prov p
    _             -> pure mempty


------------------------------------------------------------------------------
-- | Like 'mkDerivedConHypothesis', but for record patterns.
mkDerivedRecordHypothesis
    :: Provenance
    -> ConLike  -- ^ Destructing constructor
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
-- | Construct a fake variable to attach the current 'Provenance' to, and then
-- build a sub-hypothesis for the pattern match.
mkDerivedConHypothesis
    :: Provenance
    -> ConLike                   -- ^ Destructing constructor
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
    $ conLikeResTy dc args


------------------------------------------------------------------------------
-- | Build a 'Hypothesis' given an 'Id'.
mkIdHypothesis :: Id -> Provenance -> Hypothesis CType
mkIdHypothesis (splitId -> (name, ty)) prov =
  Hypothesis $ pure $ HyInfo name prov ty


------------------------------------------------------------------------------
-- | Is this hole immediately to the right of an equals sign --- and is there
-- no where clause attached to it?
--
-- It's important that there is no where clause because otherwise it gets
-- clobbered. See #2183 for an example.
--
-- This isn't a perfect check, and produces some ugly code. But it's much much
-- better than the alternative, which is to destructively modify the user's
-- AST.
isRhsHoleWithoutWhere
    :: Tracked age RealSrcSpan
    -> Tracked age TypecheckedSource
    -> Bool
isRhsHoleWithoutWhere (unTrack -> rss) (unTrack -> tcs) =
  everything (||) (mkQ False $ \case
      TopLevelRHS _ _
          (L (RealSrcSpan span _) _)
          (EmptyLocalBinds _) -> containsSpan rss span
      _                       -> False
    ) tcs


ufmSeverity :: UserFacingMessage -> MessageType
ufmSeverity NotEnoughGas            = MtInfo
ufmSeverity TacticErrors            = MtError
ufmSeverity TimedOut                = MtInfo
ufmSeverity NothingToDo             = MtInfo
ufmSeverity (InfrastructureError _) = MtError


mkShowMessageParams :: UserFacingMessage -> ShowMessageParams
mkShowMessageParams ufm = ShowMessageParams (ufmSeverity ufm) $ T.pack $ show ufm


showLspMessage :: MonadLsp cfg m => ShowMessageParams -> m ()
showLspMessage = sendNotification SWindowShowMessage


-- This rule only exists for generating file diagnostics
-- so the RuleResult is empty
data WriteDiagnostics = WriteDiagnostics
    deriving (Eq, Show, Typeable, Generic)

instance Hashable WriteDiagnostics
instance NFData   WriteDiagnostics

type instance RuleResult WriteDiagnostics = ()

wingmanRules :: Recorder (WithPriority Log) -> PluginId -> Rules ()
wingmanRules recorder plId = do
  define (cmapWithPrio LogShake recorder) $ \WriteDiagnostics nfp ->
    usePropertyAction #hole_severity plId properties >>= \case
      Nothing -> pure (mempty, Just ())
      Just severity ->
        use GetParsedModule nfp >>= \case
          Nothing ->
            pure ([], Nothing)
          Just pm -> do
            let holes :: [Range]
                holes =
                  everything (<>)
                    (mkQ mempty $ \case
                      L span (HsVar _ (L _ name))
                        | isHole (occName name) ->
                            maybeToList $ srcSpanToRange span
#if __GLASGOW_HASKELL__ >= 900
                      L span (HsUnboundVar _ occ)
#else
                      L span (HsUnboundVar _ (TrueExprHole occ))
#endif
                        | isHole occ ->
                            maybeToList $ srcSpanToRange span
                      (_ :: LHsExpr GhcPs) -> mempty
                    ) $ pm_parsed_source pm
            pure
              ( fmap (\r -> (nfp, ShowDiag, mkDiagnostic severity r)) holes
              , Just ()
              )

  action $ do
    files <- getFilesOfInterestUntracked
    void $ uses WriteDiagnostics $ Map.keys files


mkDiagnostic :: DiagnosticSeverity -> Range -> Diagnostic
mkDiagnostic severity r =
  Diagnostic r
    (Just severity)
    (Just $ InR "hole")
    (Just "wingman")
    "Hole"
    (Just $ List [DtUnnecessary])
    Nothing


------------------------------------------------------------------------------
-- | Transform a 'Graft' over the AST into a 'WorkspaceEdit'.
mkWorkspaceEdits
    :: DynFlags
    -> ClientCapabilities
    -> Uri
    -> Annotated ParsedSource
    -> Graft (Either String) ParsedSource
    -> Either UserFacingMessage WorkspaceEdit
mkWorkspaceEdits dflags ccs uri pm g = do
  let response = transform dflags ccs uri g pm
   in first (InfrastructureError . T.pack) response


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType

