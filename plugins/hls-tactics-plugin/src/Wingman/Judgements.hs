module Wingman.Judgements where

import           Control.Arrow
import           Control.Lens                        hiding (Context)
import           Data.Bool
import           Data.Char
import           Data.Coerce
import           Data.Generics.Product               (field)
import           Data.Map                            (Map)
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Set                            (Set)
import qualified Data.Set                            as S
import           DataCon                             (DataCon)
import           Development.IDE.Spans.LocalBindings
import           OccName
import           SrcLoc
import           Type
import           Wingman.Types


------------------------------------------------------------------------------
-- | Given a 'SrcSpan' and a 'Bindings', create a hypothesis.
hypothesisFromBindings :: RealSrcSpan -> Bindings -> Hypothesis CType
hypothesisFromBindings span bs = buildHypothesis $ getLocalScope bs span


------------------------------------------------------------------------------
-- | Convert a @Set Id@ into a hypothesis.
buildHypothesis :: [(Name, Maybe Type)] -> Hypothesis CType
buildHypothesis
  = Hypothesis
  . mapMaybe go
  where
    go (occName -> occ, t)
      | Just ty <- t
      , isAlpha . head . occNameString $ occ = Just $ HyInfo occ UserPrv $ CType ty
      | otherwise = Nothing


blacklistingDestruct :: Judgement -> Judgement
blacklistingDestruct =
  field @"_jBlacklistDestruct" .~ True


unwhitelistingSplit :: Judgement -> Judgement
unwhitelistingSplit =
  field @"_jWhitelistSplit" .~ False


isDestructBlacklisted :: Judgement -> Bool
isDestructBlacklisted = _jBlacklistDestruct


isSplitWhitelisted :: Judgement -> Bool
isSplitWhitelisted = _jWhitelistSplit


withNewGoal :: a -> Judgement' a -> Judgement' a
withNewGoal t = field @"_jGoal" .~ t


introduce :: Hypothesis a -> Judgement' a -> Judgement' a
introduce hy = field @"_jHypothesis" <>~ hy


------------------------------------------------------------------------------
-- | Helper function for implementing functions which introduce new hypotheses.
introduceHypothesis
    :: (Int -> Int -> Provenance)
        -- ^ A function from the total number of args and position of this arg
        -- to its provenance.
    -> [(OccName, a)]
    -> Hypothesis a
introduceHypothesis f ns =
  Hypothesis $ zip [0..] ns <&> \(pos, (name, ty)) ->
    HyInfo name (f (length ns) pos) ty


------------------------------------------------------------------------------
-- | Introduce bindings in the context of a lamba.
lambdaHypothesis
    :: Maybe OccName   -- ^ The name of the top level function. For any other
                       -- function, this should be 'Nothing'.
    -> [(OccName, a)]
    -> Hypothesis a
lambdaHypothesis func =
  introduceHypothesis $ \count pos ->
    maybe UserPrv (\x -> TopLevelArgPrv x pos count) func


------------------------------------------------------------------------------
-- | Introduce a binding in a recursive context.
recursiveHypothesis :: [(OccName, a)] -> Hypothesis a
recursiveHypothesis = introduceHypothesis $ const $ const RecursivePrv


------------------------------------------------------------------------------
-- | Check whether any of the given occnames are an ancestor of the term.
hasPositionalAncestry
    :: Foldable t
    => t OccName   -- ^ Desired ancestors.
    -> Judgement
    -> OccName     -- ^ Potential child
    -> Maybe Bool  -- ^ Just True if the result is the oldest positional ancestor
                   -- just false if it's a descendent
                   -- otherwise nothing
hasPositionalAncestry ancestors jdg name
  | not $ null ancestors
  = case any (== name) ancestors of
      True  -> Just True
      False ->
        case M.lookup name $ jAncestryMap jdg of
          Just ancestry ->
            bool Nothing (Just False) $ any (flip S.member ancestry) ancestors
          Nothing -> Nothing
  | otherwise = Nothing


------------------------------------------------------------------------------
-- | Helper function for disallowing hypotheses that have the wrong ancestry.
filterAncestry
    :: Foldable t
    => t OccName
    -> DisallowReason
    -> Judgement
    -> Judgement
filterAncestry ancestry reason jdg =
    disallowing reason (M.keys $ M.filterWithKey go $ hyByName $ jHypothesis jdg) jdg
  where
    go name _
      = not
      . isJust
      $ hasPositionalAncestry ancestry jdg name


------------------------------------------------------------------------------
-- | @filter defn pos@ removes any hypotheses which are bound in @defn@ to
-- a position other than @pos@. Any terms whose ancestry doesn't include @defn@
-- remain.
filterPosition :: OccName -> Int -> Judgement -> Judgement
filterPosition defn pos jdg =
  filterAncestry (findPositionVal jdg defn pos) (WrongBranch pos) jdg


------------------------------------------------------------------------------
-- | Helper function for determining the ancestry list for 'filterPosition'.
findPositionVal :: Judgement' a -> OccName -> Int -> Maybe OccName
findPositionVal jdg defn pos = listToMaybe $ do
  -- It's important to inspect the entire hypothesis here, as we need to trace
  -- ancstry through potentially disallowed terms in the hypothesis.
  (name, hi) <- M.toList $ M.map (overProvenance expandDisallowed) $ hyByName $ jEntireHypothesis jdg
  case hi_provenance hi of
    TopLevelArgPrv defn' pos' _
      | defn == defn'
      , pos  == pos' -> pure name
    PatternMatchPrv pv
      | pv_scrutinee pv == Just defn
      , pv_position pv  == pos -> pure name
    _ -> []


------------------------------------------------------------------------------
-- | Helper function for determining the ancestry list for
-- 'filterSameTypeFromOtherPositions'.
findDconPositionVals :: Judgement' a -> DataCon -> Int -> [OccName]
findDconPositionVals jdg dcon pos = do
  (name, hi) <- M.toList $ hyByName $ jHypothesis jdg
  case hi_provenance hi of
    PatternMatchPrv pv
      | pv_datacon  pv == Uniquely dcon
      , pv_position pv == pos -> pure name
    _ -> []


------------------------------------------------------------------------------
-- | Disallow any hypotheses who have the same type as anything bound by the
-- given position for the datacon. Used to ensure recursive functions like
-- 'fmap' preserve the relative ordering of their arguments by eliminating any
-- other term which might match.
filterSameTypeFromOtherPositions :: DataCon -> Int -> Judgement -> Judgement
filterSameTypeFromOtherPositions dcon pos jdg =
  let hy = hyByName
         . jHypothesis
         $ filterAncestry
             (findDconPositionVals jdg dcon pos)
             (WrongBranch pos)
             jdg
      tys = S.fromList $ hi_type <$> M.elems hy
      to_remove =
        M.filter (flip S.member tys . hi_type) (hyByName $ jHypothesis jdg)
          M.\\ hy
   in disallowing Shadowed (M.keys to_remove) jdg


------------------------------------------------------------------------------
-- | Return the ancestry of a 'PatVal', or 'mempty' otherwise.
getAncestry :: Judgement' a -> OccName -> Set OccName
getAncestry jdg name =
  case M.lookup name $ jPatHypothesis jdg of
    Just pv -> pv_ancestry pv
    Nothing -> mempty


jAncestryMap :: Judgement' a -> Map OccName (Set OccName)
jAncestryMap jdg =
  flip M.map (jPatHypothesis jdg) pv_ancestry


provAncestryOf :: Provenance -> Set OccName
provAncestryOf (TopLevelArgPrv o i i3) = S.singleton o
provAncestryOf (PatternMatchPrv (PatVal mo so ud i)) = maybe mempty S.singleton mo <> so
provAncestryOf (ClassMethodPrv uc) = mempty
provAncestryOf UserPrv = mempty
provAncestryOf RecursivePrv = mempty
provAncestryOf (DisallowedPrv d p2) = provAncestryOf p2


------------------------------------------------------------------------------
-- TODO(sandy): THIS THING IS A BIG BIG HACK
--
-- Why? 'ctxDefiningFuncs' is _all_ of the functions currently beind defined
-- (eg, we might be in a where block). The head of this list is not guaranteed
-- to be the one we're interested in.
extremelyStupid__definingFunction :: Context -> OccName
extremelyStupid__definingFunction =
  fst . head . ctxDefiningFuncs


patternHypothesis
    :: Maybe OccName
    -> DataCon
    -> Judgement' a
    -> [(OccName, a)]
    -> Hypothesis a
patternHypothesis scrutinee dc jdg
  = introduceHypothesis $ \_ pos ->
      PatternMatchPrv $
        PatVal
            scrutinee
            (maybe mempty
                  (\scrut -> S.singleton scrut <> getAncestry jdg scrut)
                  scrutinee)
            (Uniquely dc)
            pos


------------------------------------------------------------------------------
-- | Prevent some occnames from being used in the hypothesis. This will hide
-- them from 'jHypothesis', but not from 'jEntireHypothesis'.
disallowing :: DisallowReason -> [OccName] -> Judgement' a -> Judgement' a
disallowing reason (S.fromList -> ns) =
  field @"_jHypothesis" %~ (\z -> Hypothesis . flip fmap (unHypothesis z) $ \hi ->
    case S.member (hi_name hi) ns of
      True  -> overProvenance (DisallowedPrv reason) hi
      False -> hi
                           )


------------------------------------------------------------------------------
-- | The hypothesis, consisting of local terms and the ambient environment
-- (impors and class methods.) Hides disallowed values.
jHypothesis :: Judgement' a -> Hypothesis a
jHypothesis
  = Hypothesis
  . filter (not . isDisallowed . hi_provenance)
  . unHypothesis
  . jEntireHypothesis


------------------------------------------------------------------------------
-- | The whole hypothesis, including things disallowed.
jEntireHypothesis :: Judgement' a -> Hypothesis a
jEntireHypothesis = _jHypothesis


------------------------------------------------------------------------------
-- | Just the local hypothesis.
jLocalHypothesis :: Judgement' a -> Hypothesis a
jLocalHypothesis
  = Hypothesis
  . filter (isLocalHypothesis . hi_provenance)
  . unHypothesis
  . jHypothesis


------------------------------------------------------------------------------
-- | If we're in a top hole, the name of the defining function.
isTopHole :: Context -> Judgement' a -> Maybe OccName
isTopHole ctx =
  bool Nothing (Just $ extremelyStupid__definingFunction ctx) . _jIsTopHole


unsetIsTopHole :: Judgement' a -> Judgement' a
unsetIsTopHole = field @"_jIsTopHole" .~ False


------------------------------------------------------------------------------
-- | What names are currently in scope in the hypothesis?
hyNamesInScope :: Hypothesis a -> Set OccName
hyNamesInScope = M.keysSet . hyByName


------------------------------------------------------------------------------
-- | Are there any top-level function argument bindings in this judgement?
jHasBoundArgs :: Judgement' a -> Bool
jHasBoundArgs
  = not
  . null
  . filter (isTopLevel . hi_provenance)
  . unHypothesis
  . jLocalHypothesis


------------------------------------------------------------------------------
-- | Fold a hypothesis into a single mapping from name to info. This
-- unavoidably will cause duplicate names (things like methods) to shadow one
-- another.
hyByName :: Hypothesis a -> Map OccName (HyInfo a)
hyByName
  = M.fromList
  . fmap (hi_name &&& id)
  . unHypothesis


------------------------------------------------------------------------------
-- | Only the hypothesis members which are pattern vals
jPatHypothesis :: Judgement' a -> Map OccName PatVal
jPatHypothesis
  = M.mapMaybe (getPatVal . hi_provenance)
  . hyByName
  . jHypothesis


getPatVal :: Provenance-> Maybe PatVal
getPatVal prov =
  case prov of
    PatternMatchPrv pv -> Just pv
    _                  -> Nothing


jGoal :: Judgement' a -> a
jGoal = _jGoal


substJdg :: TCvSubst -> Judgement -> Judgement
substJdg subst = fmap $ coerce . substTy subst . coerce


mkFirstJudgement
    :: Hypothesis CType
    -> Bool  -- ^ are we in the top level rhs hole?
    -> Type
    -> Judgement' CType
mkFirstJudgement hy top goal = Judgement
  { _jHypothesis        = hy
  , _jBlacklistDestruct = False
  , _jWhitelistSplit    = True
  , _jIsTopHole         = top
  , _jGoal              = CType goal
  }


------------------------------------------------------------------------------
-- | Is this a top level function binding?
isTopLevel :: Provenance -> Bool
isTopLevel TopLevelArgPrv{} = True
isTopLevel _                = False


------------------------------------------------------------------------------
-- | Is this a local function argument, pattern match or user val?
isLocalHypothesis :: Provenance -> Bool
isLocalHypothesis UserPrv{}         = True
isLocalHypothesis PatternMatchPrv{} = True
isLocalHypothesis TopLevelArgPrv{}  = True
isLocalHypothesis _                 = False


------------------------------------------------------------------------------
-- | Is this a pattern match?
isPatternMatch :: Provenance -> Bool
isPatternMatch PatternMatchPrv{} = True
isPatternMatch _                 = False


------------------------------------------------------------------------------
-- | Was this term ever disallowed?
isDisallowed :: Provenance -> Bool
isDisallowed DisallowedPrv{} = True
isDisallowed _               = False


------------------------------------------------------------------------------
-- | Eliminates 'DisallowedPrv' provenances.
expandDisallowed :: Provenance -> Provenance
expandDisallowed (DisallowedPrv _ prv) = expandDisallowed prv
expandDisallowed prv                   = prv
