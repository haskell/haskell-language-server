{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Wingman.Tactics
  ( module Wingman.Tactics
  , runTactic
  ) where

import           Control.Applicative (Alternative(empty), (<|>))
import           Control.Lens ((&), (%~), (<>~))
import           Control.Monad (filterM, unless)
import           Control.Monad (when)
import           Control.Monad.Extra (anyM)
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Control.Monad.State.Strict (StateT(..), runStateT, execStateT)
import           Data.Bool (bool)
import           Data.Foldable
import           Data.Functor ((<&>))
import           Data.Generics.Labels ()
import           Data.List
import           Data.List.Extra (dropEnd, takeEnd)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Traversable (for)
import           Development.IDE.GHC.Compat hiding (empty)
import           GHC.Exts
import           GHC.SourceGen ((@@))
import           GHC.SourceGen.Expr
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           Wingman.CodeGen
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Machinery
import           Wingman.Naming
import           Wingman.Types


------------------------------------------------------------------------------
-- | Use something in the hypothesis to fill the hole.
assumption :: TacticsM ()
assumption = attemptOn (S.toList . allNames) assume


------------------------------------------------------------------------------
-- | Use something named in the hypothesis to fill the hole.
assume :: OccName -> TacticsM ()
assume name = rule $ \jdg -> do
  case M.lookup name $ hyByName $ jHypothesis jdg of
    Just (hi_type -> ty) -> do
      unify ty $ jGoal jdg
      pure $
        -- This slightly terrible construct is producing a mostly-empty
        -- 'Synthesized'; but there is no monoid instance to do something more
        -- reasonable for a default value.
        (pure (noLoc $ var' name))
    Nothing -> cut



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


newtype Saturation = Unsaturated Int
  deriving (Eq, Ord, Show)

pattern Saturated :: Saturation
pattern Saturated = Unsaturated 0


apply :: Saturation -> HyInfo CType -> TacticsM ()
apply (Unsaturated n) hi =  do
  jdg <- goal
  let g  = jGoal jdg
      ty = unCType $ hi_type hi
      func = hi_name hi
  ty' <- freshTyvars ty
  let (_, _, all_args, ret) = tacticsSplitFunTy ty'
      saturated_args = dropEnd n all_args
      unsaturated_args = takeEnd n all_args
  rule $ \jdg -> do
    unify g (CType $ mkVisFunTys unsaturated_args ret)
    ext
        <- fmap unzipTrace
        $ traverse ( newSubgoal
                    . flip withNewGoal jdg
                    . CType
                    . scaledThing
                    ) saturated_args
    pure $
      ext
        & #syn_val       %~ mkApply func . fmap unLoc

application :: TacticsM ()
application = overFunctions $ apply Saturated


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors.
split :: TacticsM ()
split =  do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Nothing -> failure $ GoalMismatch "split" g
    Just (dcs, _) -> choice $ fmap splitDataCon dcs



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
-- | Like 'split', but prunes any data constructors which have holes.
obvious :: TacticsM ()
obvious =  do
  pruning split $ bool (Just NoProgress) Nothing . null


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


------------------------------------------------------------------------------
-- | @matching f@ takes a function from a judgement to a @Tactic@, and
-- then applies the resulting @Tactic@.
matching :: (Judgement -> TacticsM ()) -> TacticsM ()
matching f = TacticT $ StateT $ \s -> runStateT (unTacticT $ f s) s


attemptOn :: (Judgement -> [a]) -> (a -> TacticsM ()) -> TacticsM ()
attemptOn getNames tac = matching (choice . fmap tac . getNames)


localTactic :: TacticsM a -> (Judgement -> Judgement) -> TacticsM a
localTactic t f = do
  TacticT $ StateT $ \jdg ->
    runStateT (unTacticT t) $ f jdg


refine :: TacticsM ()
refine = intros <%> splitSingle


overFunctions :: (HyInfo CType -> TacticsM ()) -> TacticsM ()
overFunctions =
  attemptOn $ filter (isFunction . unCType . hi_type)
           . unHypothesis
           . jHypothesis

overAlgebraicTerms :: (HyInfo CType -> TacticsM ()) -> TacticsM ()
overAlgebraicTerms =
  attemptOn jAcceptableDestructTargets


allNames :: Judgement -> Set OccName
allNames = hyNamesInScope . jHypothesis



applyByName :: OccName -> TacticsM ()
applyByName name = do
  g <- goal
  choice $ unHypothesis (jHypothesis g) <&> \hi ->
    case hi_name hi == name of
      True  -> apply Saturated hi
      False -> empty


bindOne :: TacticsM a -> TacticsM a -> TacticsM a
bindOne t t1 = t <@> [t1]



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


subgoalWith :: Judgement -> TacticsM () -> RuleM (Synthesized (LHsExpr GhcPs))
subgoalWith jdg t = RuleT $ flip execStateT jdg $ unTacticT t

