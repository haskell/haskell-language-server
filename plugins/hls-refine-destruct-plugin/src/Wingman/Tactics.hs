{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Wingman.Tactics
  ( module Wingman.Tactics
  , runTactic
  ) where

import           Control.Lens ((&), (%~))
import           Control.Monad (unless)
import           Data.Bool (bool)
import           Data.Foldable
import           Data.Generics.Labels ()
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Development.IDE.GHC.Compat hiding (empty)
import           GHC.Exts
import           GHC.SourceGen.Expr
import           Refinery.Tactic
import           Wingman.CodeGen
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Machinery
import           Wingman.Naming
import           Wingman.Types


------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros :: TacticsM ()
intros = intros' IntroduceAllUnnamed


data IntroParams
  = IntroduceAllUnnamed
  | IntroduceOnlyNamed [OccName]
  | IntroduceOnlyUnnamed Int
  deriving stock (Eq, Ord, Show)


------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros'
    :: IntroParams
    -> TacticsM ()
intros' params = rule $ \jdg -> do
  let g  = jGoal jdg
  case tacticsSplitFunTy $ unCType g of
    (_, _, [], _) -> cut -- failure $ GoalMismatch "intros" g
    (_, _, scaledArgs, res) -> do
      let args = fmap scaledThing scaledArgs
      let gen_names = mkManyGoodNames (hyNamesInScope $ jEntireHypothesis jdg) args
          occs = case params of
            IntroduceAllUnnamed -> gen_names
            IntroduceOnlyNamed names -> names
            IntroduceOnlyUnnamed n -> take n gen_names
          num_occs = length occs
          top_hole = _jIsTopHole jdg
          bindings = zip occs $ coerce args
          bound_occs = fmap fst bindings
          hy' = lambdaHypothesis (bool Nothing (error "TODO") top_hole) bindings
          jdg' = introduce hy'
               $ withNewGoal (CType $ mkVisFunTys (drop num_occs scaledArgs) res) jdg
      ext <- newSubgoal jdg'
      pure $
        ext
          & #syn_val   %~ noLoc . lambda (fmap bvar' bound_occs) . unLoc


------------------------------------------------------------------------------
-- | Introduce a single lambda argument, and immediately destruct it.
introAndDestruct :: TacticsM ()
introAndDestruct = do
  hy <- fmap unHypothesis $ hyDiff $ intros' $ IntroduceOnlyUnnamed 1
  -- This case should never happen, but I'm validating instead of parsing.
  -- Adding a log to be reminded if the invariant ever goes false.
  --
  -- But note that this isn't a game-ending bug. In the worst case, we'll
  -- accidentally bind too many variables, and incorrectly unify between them.
  -- Which means some GADT cases that should be eliminated won't be --- not the
  -- end of the world.
  unless (length hy == 1) $
    traceMX "BUG: Introduced too many variables for introAndDestruct! Please report me if you see this! " hy

  for_ hy destruct


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destruct :: HyInfo CType -> TacticsM ()
destruct hi = requireConcreteHole $
  rule $ destruct' False (const newSubgoal) hi


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches. Performs record punning.
destructPun :: HyInfo CType -> TacticsM ()
destructPun hi = requireConcreteHole $
  rule $ destruct' True (const newSubgoal) hi


------------------------------------------------------------------------------
-- | Case split, using the same data constructor in the matches.
homo :: HyInfo CType -> TacticsM ()
homo hi = requireConcreteHole $ do
  jdg <- goal
  let g = jGoal jdg

  -- Ensure that every data constructor in the domain type is covered in the
  -- codomain; otherwise 'homo' will produce an ill-typed program.
  case uncoveredDataCons (coerce $ hi_type hi) (coerce g) of
    Just uncovered_dcs ->
      unless (S.null uncovered_dcs) $
        failure  $ TacticPanic "Can't cover every datacon in domain"
    _ -> failure $ TacticPanic "Unable to fetch datacons"

  rule
    $ destruct'
        False
        (\dc jdg -> buildDataCon jdg dc $ snd $ splitAppTys $ unCType $ jGoal jdg)
        hi


------------------------------------------------------------------------------
-- | LambdaCase split, and leave holes in the matches.
destructLambdaCase :: TacticsM ()
destructLambdaCase =
  rule $ destructLambdaCase' False (const newSubgoal)


------------------------------------------------------------------------------
-- | LambdaCase split, using the same data constructor in the matches.
homoLambdaCase :: TacticsM ()
homoLambdaCase =
  rule $ destructLambdaCase' False $ \dc jdg ->
    buildDataCon jdg dc
      . snd
      . splitAppTys
      . unCType
      $ jGoal jdg


------------------------------------------------------------------------------
-- | Like 'split', but only works if there is a single matching data
-- constructor for the goal.
splitSingle :: TacticsM ()
splitSingle =  do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Just ([dc], _) -> do
      splitDataCon dc
    _ -> failure $ GoalMismatch "splitSingle" g


------------------------------------------------------------------------------
-- | Attempt to instantiate the given ConLike to solve the goal.
--
-- INVARIANT: Assumes the given ConLike is appropriate to construct the type
-- with.
splitConLike :: ConLike -> TacticsM ()
splitConLike dc =
  requireConcreteHole $  rule $ \jdg -> do
    let g = jGoal jdg
    case splitTyConApp_maybe $ unCType g of
      Just (_, apps) -> do
        buildDataCon jdg dc apps
      Nothing -> cut -- failure $ GoalMismatch "splitDataCon" g


------------------------------------------------------------------------------
-- | Attempt to instantiate the given data constructor to solve the goal.
--
-- INVARIANT: Assumes the given datacon is appropriate to construct the type
-- with.
splitDataCon :: DataCon -> TacticsM ()
splitDataCon = splitConLike . RealDataCon


------------------------------------------------------------------------------
-- | Perform a case split on each top-level argument. Used to implement the
-- "Destruct all function arguments" action.
destructAll :: TacticsM ()
destructAll = do
  jdg <- goal
  let args = fmap fst
           $ sortOn snd
           $ mapMaybe (\(hi, prov) ->
              case prov of
                TopLevelArgPrv _ idx _ -> pure (hi, idx)
                _ -> Nothing
                )
           $ fmap (\hi -> (hi, hi_provenance hi))
           $ filter (isAlgType . unCType . hi_type)
           $ unHypothesis
           $ jHypothesis jdg
  for_ args $ \arg -> do
    subst <- getSubstForJudgement =<< goal
    destruct $ fmap (coerce substTy subst) arg


--------------------------------------------------------------------------------
-- | User-facing tactic to implement "Use constructor <x>"
userSplit :: OccName -> TacticsM ()
userSplit occ = do
  jdg <- goal
  let g = jGoal jdg
  -- TODO(sandy): It's smelly that we need to find the datacon to generate the
  -- code action, send it as a string, and then look it up again. Can we push
  -- this over LSP somehow instead?
  case splitTyConApp_maybe $ unCType g of
    Just (tc, _) -> do
      case find (sloppyEqOccName occ . occName . dataConName)
             $ tyConDataCons tc of
        Just dc -> splitDataCon dc
        Nothing -> failure $ NotInScope occ
    Nothing -> failure $ NotInScope occ


refine :: TacticsM ()
refine = intros <%> splitSingle


------------------------------------------------------------------------------
-- | Determine the difference in hypothesis due to running a tactic. Also, it
-- runs the tactic.
hyDiff :: TacticsM () -> TacticsM (Hypothesis CType)
hyDiff m = do
  g <- unHypothesis . jEntireHypothesis <$> goal
  let g_len = length g
  m
  g' <- unHypothesis . jEntireHypothesis <$> goal
  pure $ Hypothesis $ take (length g' - g_len) g'

