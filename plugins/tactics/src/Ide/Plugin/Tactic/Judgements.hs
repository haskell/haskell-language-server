{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Ide.Plugin.Tactic.Judgements where

import           Control.Lens hiding (Context)
import           Data.Bool
import           Data.Char
import           Data.Coerce
import           Data.Generics.Product (field)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           DataCon (DataCon)
import           Development.IDE.Spans.LocalBindings
import           Ide.Plugin.Tactic.Types
import           OccName
import           SrcLoc
import           Type


------------------------------------------------------------------------------
-- | Given a 'SrcSpan' and a 'Bindings', create a hypothesis.
hypothesisFromBindings :: RealSrcSpan -> Bindings -> Map OccName CType
hypothesisFromBindings span bs = buildHypothesis $ getLocalScope bs span


------------------------------------------------------------------------------
-- | Convert a @Set Id@ into a hypothesis.
buildHypothesis :: [(Name, Maybe Type)] -> Map OccName CType
buildHypothesis
  = M.fromList
  . mapMaybe go
  where
    go (occName -> occ, t)
      | Just ty <- t
      , isAlpha . head . occNameString $ occ = Just (occ, CType ty)
      | otherwise = Nothing


hasDestructed :: Judgement' a -> OccName -> Bool
hasDestructed jdg n = flip any (jPatHypothesis jdg) $
  (== Just n) . pv_scrutinee


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


introducingLambda
    :: Maybe OccName   -- ^ top level function, or Nothing for any other function
    -> [(OccName, a)]
    -> Judgement' a
    -> Judgement' a
introducingLambda func ns =
  field @"_jHypothesis" <>~ M.fromList (zip [0..] ns <&> \(pos, (name, ty)) ->
    -- TODO(sandy): cleanup
    (name, HyInfo (maybe UserPrv (\x -> TopLevelArgPrv x pos) func) ty))


------------------------------------------------------------------------------
-- | Add some terms to the ambient hypothesis
introducingRecursively :: [(OccName, a)] -> Judgement' a -> Judgement' a
introducingRecursively ns =
  field @"_jHypothesis" <>~ M.fromList (ns <&> \(name, ty) ->
    -- TODO(sandy): cleanup
    (name, HyInfo RecursivePrv ty
    ))


filterPosition :: OccName -> Int -> Judgement -> Judgement
filterPosition defn pos jdg =
    withHypothesis (M.filterWithKey go) jdg
  where
    go name _ = isJust $ hasPositionalAncestry jdg defn pos name


filterSameTypeFromOtherPositions :: OccName -> Int -> Judgement -> Judgement
filterSameTypeFromOtherPositions defn pos jdg =
  let hy = jHypothesis $ filterPosition defn pos jdg
      tys = S.fromList $ fmap snd $ M.toList hy
   in withHypothesis (\hy2 -> M.filter (not . flip S.member tys) hy2 <> hy) jdg


getAncestry :: Judgement' a -> OccName -> Set OccName
getAncestry jdg name =
  case M.lookup name $ jPatHypothesis jdg of
    Just pv -> pv_ancestry pv
    Nothing -> mempty


hasPositionalAncestry
    :: Judgement
    -> OccName     -- ^ defining fn
    -> Int         -- ^ position
    -> OccName     -- ^ thing to check ancestry
    -> Maybe Bool  -- ^ Just True if the result is the oldest positional ancestor
                   -- just false if it's a descendent
                   -- otherwise nothing
hasPositionalAncestry jdg defn n name
  | not $ null ancestors
  = case any (== name) ancestors of
      True  -> Just True
      False ->
        case M.lookup name $ jAncestryMap jdg of
          Just ancestry ->
            bool Nothing (Just False) $ any (flip S.member ancestry) ancestors
          Nothing -> Nothing
  | otherwise = Nothing
  where
    ancestors = toListOf (_Just . traversed . ix n)
              $ M.lookup defn
              $ _jPositionMaps jdg


jAncestryMap :: Judgement' a -> Map OccName (Set OccName)
jAncestryMap jdg =
  flip M.map (jPatHypothesis jdg) pv_ancestry


withPositionMapping :: OccName -> [OccName] -> Judgement -> Judgement
withPositionMapping defn names =
  field @"_jPositionMaps" . at defn <>~ Just [names]


------------------------------------------------------------------------------
-- TODO(sandy): THIS THING IS A BIG BIG HACK
--
-- Why? 'ctxDefiningFuncs' is _all_ of the functions currently beind defined
-- (eg, we might be in a where block). The head of this list is not guaranteed
-- to be the one we're interested in.
extremelyStupid__definingFunction :: Context -> OccName
extremelyStupid__definingFunction =
  fst . head . ctxDefiningFuncs


withHypothesis
    :: (Map OccName (HyInfo a) -> Map OccName (HyInfo a))
    -> Judgement' a
    -> Judgement' a
withHypothesis f =
  field @"_jHypothesis" %~ f


------------------------------------------------------------------------------
-- | Pattern vals are currently tracked in jHypothesis, with an extra piece of
-- data sitting around in jPatternVals.
introducingPat
    :: Maybe OccName
    -> DataCon
    -> [(OccName, a)]
    -> Judgement' a
    -> Judgement' a
introducingPat scrutinee dc ns jdg = jdg
  & field @"_jHypothesis"  <>~ (M.fromList $ zip [0..] ns <&> \(pos, (name, ty)) ->
      ( name
      , HyInfo
          (PatternMatchPrv $ PatVal
              scrutinee
              (maybe
                  mempty
                  (\scrut -> S.singleton scrut <> getAncestry jdg scrut)
                  scrutinee)
              (Uniquely dc)
              pos)
          ty))


disallowing :: [OccName] -> Judgement' a -> Judgement' a
disallowing ns =
  field @"_jHypothesis" %~ flip M.withoutKeys (S.fromList ns)


------------------------------------------------------------------------------
-- | The hypothesis, consisting of local terms and the ambient environment
-- (includes and class methods.)
jHypothesis :: Judgement' a -> Map OccName (HyInfo a)
jHypothesis = _jHypothesis


------------------------------------------------------------------------------
-- | Just the local hypothesis.
jLocalHypothesis :: Judgement' a -> Map OccName (HyInfo a)
jLocalHypothesis = M.filter (isLocalHypothesis . hi_provenance) . _jHypothesis


isTopHole :: Context -> Judgement' a -> Maybe OccName
isTopHole ctx =
  bool Nothing (Just $ extremelyStupid__definingFunction ctx) . _jIsTopHole

unsetIsTopHole :: Judgement' a -> Judgement' a
unsetIsTopHole = field @"_jIsTopHole" .~ False


------------------------------------------------------------------------------
-- | Only the hypothesis members which are pattern vals
jPatHypothesis :: Judgement' a -> Map OccName PatVal
jPatHypothesis = M.mapMaybe (getPatVal . hi_provenance) . _jHypothesis


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
    :: M.Map OccName CType  -- ^ local hypothesis
    -> M.Map OccName CType  -- ^ ambient hypothesis
    -> Bool  -- ^ are we in the top level rhs hole?
    -> M.Map OccName [[OccName]]  -- ^ existing pos vals
    -> Type
    -> Judgement' CType
mkFirstJudgement hy ambient top posvals goal = Judgement
  { _jHypothesis        = M.map mkLocalHypothesisInfo hy
                       <> M.map mkAmbientHypothesisInfo ambient
  , _jBlacklistDestruct = False
  , _jWhitelistSplit    = True
  , _jPositionMaps      = posvals
  , _jIsTopHole         = top
  , _jGoal              = CType goal
  }


mkLocalHypothesisInfo :: a -> HyInfo a
mkLocalHypothesisInfo = HyInfo UserPrv


mkAmbientHypothesisInfo :: a -> HyInfo a
mkAmbientHypothesisInfo = HyInfo ImportPrv


isLocalHypothesis :: Provenance -> Bool
isLocalHypothesis UserPrv{} = True
isLocalHypothesis PatternMatchPrv{} = True
isLocalHypothesis TopLevelArgPrv{} = True
isLocalHypothesis _ = False


isPatternMatch :: Provenance -> Bool
isPatternMatch PatternMatchPrv{} = True
isPatternMatch _ = False

