{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Ide.Plugin.Tactic.Judgements
  ( blacklistingDestruct
  , unwhitelistingSplit
  , introducingLambda
  , introducingRecursively
  , introducingPat
  , jGoal
  , jHypothesis
  , jPatHypothesis
  , substJdg
  , unsetIsTopHole
  , filterSameTypeFromOtherPositions
  , isDestructBlacklisted
  , withNewGoal
  , jLocalHypothesis
  , hasDestructed
  , isSplitWhitelisted
  , isPatternMatch
  , filterPosition
  , isTopHole
  , disallowing
  , mkFirstJudgement
  , hypothesisFromBindings
  , isTopLevel
  ) where

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
hypothesisFromBindings :: RealSrcSpan -> Bindings -> Map OccName (HyInfo CType)
hypothesisFromBindings span bs = buildHypothesis $ getLocalScope bs span


------------------------------------------------------------------------------
-- | Convert a @Set Id@ into a hypothesis.
buildHypothesis :: [(Name, Maybe Type)] -> Map OccName (HyInfo CType)
buildHypothesis
  = M.fromList
  . mapMaybe go
  where
    go (occName -> occ, t)
      | Just ty <- t
      , isAlpha . head . occNameString $ occ = Just (occ, HyInfo UserPrv $ CType ty)
      | otherwise = Nothing


hasDestructed :: Judgement' a -> OccName -> Bool
hasDestructed jdg n = S.member n $ _jDestructed jdg


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


introducing
    :: (Int -> Provenance)
    -> [(OccName, a)]
    -> Judgement' a
    -> Judgement' a
introducing f ns =
  field @"_jHypothesis" <>~ M.fromList (zip [0..] ns <&>
    \(pos, (name, ty)) -> (name, HyInfo (f pos) ty))


introducingLambda
    :: Maybe OccName   -- ^ top level function, or Nothing for any other function
    -> [(OccName, a)]
    -> Judgement' a
    -> Judgement' a
introducingLambda func = introducing $ \pos ->
  maybe UserPrv (\x -> TopLevelArgPrv x pos) func


introducingRecursively :: [(OccName, a)] -> Judgement' a -> Judgement' a
introducingRecursively = introducing $ const RecursivePrv


hasPositionalAncestry
    :: Foldable t
    => t OccName
    -> Judgement
    -> OccName     -- ^ thing to check ancestry
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


filterAncestry
    :: Foldable t
    => t OccName
    -> DisallowReason
    -> Judgement
    -> Judgement
filterAncestry ancestry reason jdg =
    disallowing reason (M.keys $ M.filterWithKey go $ jHypothesis jdg) jdg
  where
    go name _
      = not
      . isJust
      $ hasPositionalAncestry ancestry jdg name

filterPosition :: OccName -> Int -> Judgement -> Judgement
filterPosition defn pos jdg =
  filterAncestry (findPositionVal jdg defn pos) (WrongBranch pos) jdg


findPositionVal :: Judgement' a -> OccName -> Int -> Maybe OccName
findPositionVal jdg defn pos = listToMaybe $ do
  -- It's important to inspect the entire hypothesis here, as we need to trace
  -- ancstry through potentially disallowed terms in the hypothesis.
  (name, hi) <- M.toList $ M.map (overProvenance expandDisallowed) $ jEntireHypothesis jdg
  case hi_provenance hi of
    TopLevelArgPrv defn' pos'
      | defn == defn'
      , pos  == pos' -> pure name
    PatternMatchPrv pv
      | pv_scrutinee pv == Just defn
      , pv_position pv  == pos -> pure name
    _ -> []

findDconPositionVals :: Judgement' a -> DataCon -> Int -> [OccName]
findDconPositionVals jdg dcon pos = do
  (name, hi) <- M.toList $ jHypothesis jdg
  case hi_provenance hi of
    PatternMatchPrv pv
      | pv_datacon  pv == Uniquely dcon
      , pv_position pv == pos -> pure name
    _ -> []


filterSameTypeFromOtherPositions :: DataCon -> Int -> Judgement -> Judgement
filterSameTypeFromOtherPositions dcon pos jdg =
  let hy = jHypothesis
         $ filterAncestry
             (findDconPositionVals jdg dcon pos)
             (WrongBranch pos)
             jdg
      tys = S.fromList $ fmap (hi_type . snd) $ M.toList hy
      to_remove =
        M.filter (flip S.member tys . hi_type) (jHypothesis jdg)
          M.\\ hy
   in disallowing Shadowed (M.keys to_remove) jdg



getAncestry :: Judgement' a -> OccName -> Set OccName
getAncestry jdg name =
  case M.lookup name $ jPatHypothesis jdg of
    Just pv -> pv_ancestry pv
    Nothing -> mempty


jAncestryMap :: Judgement' a -> Map OccName (Set OccName)
jAncestryMap jdg =
  flip M.map (jPatHypothesis jdg) pv_ancestry


------------------------------------------------------------------------------
-- TODO(sandy): THIS THING IS A BIG BIG HACK
--
-- Why? 'ctxDefiningFuncs' is _all_ of the functions currently beind defined
-- (eg, we might be in a where block). The head of this list is not guaranteed
-- to be the one we're interested in.
extremelyStupid__definingFunction :: Context -> OccName
extremelyStupid__definingFunction =
  fst . head . ctxDefiningFuncs


------------------------------------------------------------------------------
-- | Pattern vals are currently tracked in jHypothesis, with an extra piece of
-- data sitting around in jPatternVals.
introducingPat
    :: Maybe OccName
    -> DataCon
    -> [(OccName, a)]
    -> Judgement' a
    -> Judgement' a
introducingPat scrutinee dc ns jdg
  = maybe id (\scrut -> field @"_jDestructed" <>~ S.singleton scrut) scrutinee
  $ introducing (\pos ->
      PatternMatchPrv $
        PatVal
          scrutinee
          (maybe mempty
                (\scrut -> S.singleton scrut <> getAncestry jdg scrut)
                scrutinee)
          (Uniquely dc)
          pos
    ) ns jdg


disallowing :: DisallowReason -> [OccName] -> Judgement' a -> Judgement' a
disallowing reason (S.fromList -> ns) =
  field @"_jHypothesis" %~ (M.mapWithKey $ \name hi ->
    case S.member name ns of
      True -> overProvenance (DisallowedPrv reason) hi
      False -> hi
                           )


------------------------------------------------------------------------------
-- | The hypothesis, consisting of local terms and the ambient environment
-- (includes and class methods.)
jHypothesis :: Judgement' a -> Map OccName (HyInfo a)
jHypothesis = M.filter (not . isDisallowed . hi_provenance) . jEntireHypothesis


------------------------------------------------------------------------------
-- | The whole hypothesis, including things disallowed.
jEntireHypothesis :: Judgement' a -> Map OccName (HyInfo a)
jEntireHypothesis = _jHypothesis


------------------------------------------------------------------------------
-- | Just the local hypothesis.
jLocalHypothesis :: Judgement' a -> Map OccName (HyInfo a)
jLocalHypothesis = M.filter (isLocalHypothesis . hi_provenance) . jHypothesis


isTopHole :: Context -> Judgement' a -> Maybe OccName
isTopHole ctx =
  bool Nothing (Just $ extremelyStupid__definingFunction ctx) . _jIsTopHole

unsetIsTopHole :: Judgement' a -> Judgement' a
unsetIsTopHole = field @"_jIsTopHole" .~ False


------------------------------------------------------------------------------
-- | Only the hypothesis members which are pattern vals
jPatHypothesis :: Judgement' a -> Map OccName PatVal
jPatHypothesis = M.mapMaybe (getPatVal . hi_provenance) . jHypothesis


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
    :: M.Map OccName (HyInfo CType)
    -> Bool  -- ^ are we in the top level rhs hole?
    -> Type
    -> Judgement' CType
mkFirstJudgement hy top goal = Judgement
  { _jHypothesis        = hy
  , _jBlacklistDestruct = False
  , _jWhitelistSplit    = True
  , _jDestructed        = mempty
  , _jIsTopHole         = top
  , _jGoal              = CType goal
  }


isTopLevel :: Provenance -> Bool
isTopLevel TopLevelArgPrv{} = True
isTopLevel _                = False

isLocalHypothesis :: Provenance -> Bool
isLocalHypothesis UserPrv{}         = True
isLocalHypothesis PatternMatchPrv{} = True
isLocalHypothesis TopLevelArgPrv{}  = True
isLocalHypothesis _                 = False


isPatternMatch :: Provenance -> Bool
isPatternMatch PatternMatchPrv{} = True
isPatternMatch _                 = False


isDisallowed :: Provenance -> Bool
isDisallowed DisallowedPrv{} = True
isDisallowed _               = False


expandDisallowed :: Provenance -> Provenance
expandDisallowed (DisallowedPrv _ prv) = expandDisallowed prv
expandDisallowed prv = prv

