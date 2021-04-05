module Wingman.Tactics
  ( module Wingman.Tactics
  , runTactic
  ) where

import           ConLike (ConLike(RealDataCon))
import           Control.Applicative (Alternative(empty))
import           Control.Lens ((&), (%~), (<>~))
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Control.Monad.State.Strict (StateT(..), runStateT)
import           Data.Foldable
import           Data.Generics.Labels ()
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           Name (occNameString, occName)
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           TcType
import           Type hiding (Var)
import           Wingman.CodeGen
import           Wingman.Context
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Machinery
import           Wingman.Naming
import           Wingman.Types
import InstEnv (InstMatch, ClsInst (is_cls, is_dfun))
import TysPrim (alphaTys)


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
          { syn_trace = tracePrim $ "assume " <> occNameString name
          , syn_used_vals = S.singleton name
          }
    Nothing -> throwError $ UndefinedHypothesis name


recursion :: TacticsM ()
-- TODO(sandy): This tactic doesn't fire for the @AutoThetaFix@ golden test,
-- presumably due to running afoul of 'requireConcreteHole'. Look into this!
recursion = requireConcreteHole $ tracing "recursion" $ do
  defs <- getCurrentDefinitions
  attemptOn (const defs) $ \(name, ty) -> markRecursion $ do
    -- Peek allows us to look at the extract produced by this block.
    peek $ \ext -> do
      jdg <- goal
      let pat_vals = jPatHypothesis jdg
      -- Make sure that the recursive call contains at least one already-bound
      -- pattern value. This ensures it is structurally smaller, and thus
      -- suggests termination.
      unless (any (flip M.member pat_vals) $ syn_used_vals ext) empty

    let hy' = recursiveHypothesis defs
    localTactic (apply $ HyInfo name RecursivePrv ty) (introduce hy')
      <@> fmap (localTactic assumption . filterPosition name) [0..]


------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros :: TacticsM ()
intros = rule $ \jdg -> do
  let g  = jGoal jdg
  ctx <- ask
  case tcSplitFunTys $ unCType g of
    ([], _) -> throwError $ GoalMismatch "intros" g
    (as, b) -> do
      vs <- mkManyGoodNames (hyNamesInScope $ jEntireHypothesis jdg) as
      let top_hole = isTopHole ctx jdg
          hy' = lambdaHypothesis top_hole $ zip vs $ coerce as
          jdg' = introduce hy'
               $ withNewGoal (CType b) jdg
      ext <- newSubgoal jdg'
      pure $
        ext
          & #syn_trace %~ rose ("intros {" <> intercalate ", " (fmap show vs) <> "}")
                        . pure
          & #syn_scoped <>~ hy'
          & #syn_val   %~ noLoc . lambda (fmap bvar' vs) . unLoc


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destructAuto :: HyInfo CType -> TacticsM ()
destructAuto hi = requireConcreteHole $ tracing "destruct(auto)" $ do
  jdg <- goal
  let subtactic = destructOrHomoAuto hi
  case isPatternMatch $ hi_provenance hi of
    True ->
      pruning subtactic $ \jdgs ->
        let getHyTypes = S.fromList . fmap hi_type . unHypothesis . jHypothesis
            new_hy = foldMap getHyTypes jdgs
            old_hy = getHyTypes jdg
        in case S.null $ new_hy S.\\ old_hy of
              True  -> Just $ UnhelpfulDestruct $ hi_name hi
              False -> Nothing
    False -> subtactic


------------------------------------------------------------------------------
-- | When running auto, in order to prune the auto search tree, we try
-- a homomorphic destruct whenever possible. If that produces any results, we
-- can probably just prune the other side.
destructOrHomoAuto :: HyInfo CType -> TacticsM ()
destructOrHomoAuto hi = tracing "destructOrHomoAuto" $ do
  jdg <- goal
  let g  = unCType $ jGoal jdg
      ty = unCType $ hi_type hi

  attemptWhen
      (rule $ destruct' (\dc jdg ->
        buildDataCon False jdg dc $ snd $ splitAppTys g) hi)
      (rule $ destruct' (const subgoal) hi)
    $ case (splitTyConApp_maybe g, splitTyConApp_maybe ty) of
        (Just (gtc, _), Just (tytc, _)) -> gtc == tytc
        _ -> False


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destruct :: HyInfo CType -> TacticsM ()
destruct hi = requireConcreteHole $ tracing "destruct(user)" $
  rule $ destruct' (const subgoal) hi


------------------------------------------------------------------------------
-- | Case split, using the same data constructor in the matches.
homo :: HyInfo CType -> TacticsM ()
homo = requireConcreteHole . tracing "homo" . rule . destruct' (\dc jdg ->
  buildDataCon False jdg dc $ snd $ splitAppTys $ unCType $ jGoal jdg)


------------------------------------------------------------------------------
-- | LambdaCase split, and leave holes in the matches.
destructLambdaCase :: TacticsM ()
destructLambdaCase = tracing "destructLambdaCase" $ rule $ destructLambdaCase' (const subgoal)


------------------------------------------------------------------------------
-- | LambdaCase split, using the same data constructor in the matches.
homoLambdaCase :: TacticsM ()
homoLambdaCase =
  tracing "homoLambdaCase" $
    rule $ destructLambdaCase' $ \dc jdg ->
      buildDataCon False jdg dc
        . snd
        . splitAppTys
        . unCType
        $ jGoal jdg


apply :: HyInfo CType -> TacticsM ()
apply hi = requireConcreteHole $ tracing ("apply' " <> show (hi_name hi)) $ do
  jdg <- goal
  let g  = jGoal jdg
      ty = unCType $ hi_type hi
      func = hi_name hi
  ty' <- freshTyvars ty
  let (_, _, args, ret) = tacticsSplitFunTy ty'
  rule $ \jdg -> do
    unify g (CType ret)
    ext
        <- fmap unzipTrace
        $ traverse ( newSubgoal
                    . blacklistingDestruct
                    . flip withNewGoal jdg
                    . CType
                    ) args
    pure $
      ext
        & #syn_used_vals %~ S.insert func
        & #syn_val       %~ noLoc . foldl' (@@) (var' func) . fmap unLoc


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors.
split :: TacticsM ()
split = tracing "split(user)" $ do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (dcs, _) -> choice $ fmap splitDataCon dcs


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors. Different than
-- 'split' because it won't split a data con if it doesn't result in any new
-- goals.
splitAuto :: TacticsM ()
splitAuto = requireConcreteHole $ tracing "split(auto)" $ do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (dcs, _) -> do
      case isSplitWhitelisted jdg of
        True -> choice $ fmap splitDataCon dcs
        False -> do
          choice $ flip fmap dcs $ \dc -> requireNewHoles $
            splitDataCon dc


------------------------------------------------------------------------------
-- | Like 'split', but only works if there is a single matching data
-- constructor for the goal.
splitSingle :: TacticsM ()
splitSingle = tracing "splitSingle" $ do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Just ([dc], _) -> do
      splitDataCon dc
    _ -> throwError $ GoalMismatch "splitSingle" g


------------------------------------------------------------------------------
-- | Allow the given tactic to proceed if and only if it introduces holes that
-- have a different goal than current goal.
requireNewHoles :: TacticsM () -> TacticsM ()
requireNewHoles m = do
  jdg <- goal
  pruning m $ \jdgs ->
    case null jdgs || any (/= jGoal jdg) (fmap jGoal jdgs) of
      True  -> Nothing
      False -> Just NoProgress


------------------------------------------------------------------------------
-- | Attempt to instantiate the given ConLike to solve the goal.
--
-- INVARIANT: Assumes the given ConLike is appropriate to construct the type
-- with.
splitConLike :: ConLike -> TacticsM ()
splitConLike dc =
  requireConcreteHole $ tracing ("splitDataCon:" <> show dc) $ rule $ \jdg -> do
    let g = jGoal jdg
    case splitTyConApp_maybe $ unCType g of
      Just (_, apps) -> do
        buildDataCon True (unwhitelistingSplit jdg) dc apps
      Nothing -> throwError $ GoalMismatch "splitDataCon" g

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
           $ sortOn (Down . snd)
           $ mapMaybe (\(hi, prov) ->
              case prov of
                TopLevelArgPrv _ idx _ -> pure (hi, idx)
                _ -> Nothing
                )
           $ fmap (\hi -> (hi, hi_provenance hi))
           $ unHypothesis
           $ jHypothesis jdg
  for_ args destruct

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
        Nothing -> throwError $ NotInScope occ
    Nothing -> throwError $ NotInScope occ


------------------------------------------------------------------------------
-- | @matching f@ takes a function from a judgement to a @Tactic@, and
-- then applies the resulting @Tactic@.
matching :: (Judgement -> TacticsM ()) -> TacticsM ()
matching f = TacticT $ StateT $ \s -> runStateT (unTacticT $ f s) s


attemptOn :: (Judgement -> [a]) -> (a -> TacticsM ()) -> TacticsM ()
attemptOn getNames tac = matching (choice . fmap (\s -> tac s) . getNames)


localTactic :: TacticsM a -> (Judgement -> Judgement) -> TacticsM a
localTactic t f = do
  TacticT $ StateT $ \jdg ->
    runStateT (unTacticT t) $ f jdg


refine :: TacticsM ()
refine = do
  try' intros
  try' splitSingle
  try' intros


auto' :: Int -> TacticsM ()
auto' 0 = throwError NoProgress
auto' n = do
  let loop = auto' (n - 1)
  try intros
  choice
    [ overFunctions $ \fname -> do
        apply fname
        loop
    , overAlgebraicTerms $ \aname -> do
        destructAuto aname
        loop
    , splitAuto >> loop
    , assumption >> loop
    , recursion
    ]

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


applyMethod :: InstMatch -> OccName -> TacticsM ()
applyMethod (inst, mapps) method_name = do
  traceMX "applying method" method_name
  let cls  = is_cls inst
  case find ((== method_name) . occName) $ classMethods cls of
    Just method -> do
      -- Get the instantiated type of the dictionary
      let df = piResultTys (idType $ is_dfun inst) $ zipWith fromMaybe alphaTys mapps
      -- pull off its resulting arguments
      let (_, apps) = splitAppTys df
      -- and apply them to the type of the instance
      let ty = snd $ splitFunTy (piResultTys (idType method) apps)
      apply $ HyInfo method_name (ClassMethodPrv $ Uniquely cls) $ CType ty
    Nothing -> do
      traceMX "no method" $ classMethods cls
      throwError $ NotInScope method_name

