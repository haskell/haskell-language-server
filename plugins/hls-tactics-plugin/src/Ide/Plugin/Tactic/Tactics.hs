{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# LANGUAGE TypeApplications #-}
module Ide.Plugin.Tactic.Tactics
  ( module Ide.Plugin.Tactic.Tactics
  , runTactic
  ) where

import           Control.Applicative (Alternative(empty))
import           Control.Lens ((&), (%~))
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
import           Ide.Plugin.Tactic.CodeGen
import           Ide.Plugin.Tactic.Context
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Machinery
import           Ide.Plugin.Tactic.Naming
import           Ide.Plugin.Tactic.Types
import           Name (occNameString)
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           TcType
import           Type hiding (Var)


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
          & #syn_val   %~ noLoc . lambda (fmap bvar' vs) . unLoc


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destructAuto :: HyInfo CType -> TacticsM ()
destructAuto hi = requireConcreteHole $ tracing "destruct(auto)" $ do
  jdg <- goal
  let subtactic = rule $ destruct' (const subgoal) hi
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
-- | Case split, and leave holes in the matches.
destruct :: HyInfo CType -> TacticsM ()
destruct hi = requireConcreteHole $ tracing "destruct(user)" $
  rule $ destruct' (const subgoal) hi


------------------------------------------------------------------------------
-- | Case split, using the same data constructor in the matches.
homo :: HyInfo CType -> TacticsM ()
homo = requireConcreteHole . tracing "homo" . rule . destruct' (\dc jdg ->
  buildDataCon jdg dc $ snd $ splitAppTys $ unCType $ jGoal jdg)


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
      buildDataCon jdg dc
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
  -- TODO(sandy): Bug here! Prevents us from doing mono-map like things
  -- Don't require new holes for locally bound vars; only respect linearity
  -- see https://github.com/haskell/haskell-language-server/issues/1447
  requireNewHoles $ rule $ \jdg -> do
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
  case splitTyConApp_maybe $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (tc, _) -> do
      let dcs = tyConDataCons tc
      choice $ fmap splitDataCon dcs


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors. Different than
-- 'split' because it won't split a data con if it doesn't result in any new
-- goals.
splitAuto :: TacticsM ()
splitAuto = requireConcreteHole $ tracing "split(auto)" $ do
  jdg <- goal
  let g = jGoal jdg
  case splitTyConApp_maybe $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (tc, _) -> do
      let dcs = tyConDataCons tc
      case isSplitWhitelisted jdg of
        True -> choice $ fmap splitDataCon dcs
        False -> do
          choice $ flip fmap dcs $ \dc -> requireNewHoles $
            splitDataCon dc


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
-- | Attempt to instantiate the given data constructor to solve the goal.
splitDataCon :: DataCon -> TacticsM ()
splitDataCon dc =
  requireConcreteHole $ tracing ("splitDataCon:" <> show dc) $ rule $ \jdg -> do
    let g = jGoal jdg
    case splitTyConApp_maybe $ unCType g of
      Just (tc, apps) -> do
        case elem dc $ tyConDataCons tc of
          True  -> buildDataCon (unwhitelistingSplit jdg) dc apps
          False -> throwError $ IncorrectDataCon dc
      Nothing -> throwError $ GoalMismatch "splitDataCon" g


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
  attemptOn $ filter (isJust . algebraicTyCon . unCType . hi_type)
            . unHypothesis
            . jHypothesis


allNames :: Judgement -> Set OccName
allNames = hyNamesInScope . jHypothesis

