{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Tactic.Tactics
  ( module Ide.Plugin.Tactic.Tactics
  , runTactic
  ) where


import           Control.Applicative
import           Control.Monad.State.Strict (StateT(..), runStateT)
import           Control.Monad.Except (throwError)
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Map as M
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           Ide.Plugin.Tactic.Machinery

import           Name
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           TcType
import           Type hiding (Var)


------------------------------------------------------------------------------
-- | Like 'var', but works over standard GHC 'OccName's.
var' :: Var a => OccName -> a
var' = var . fromString . occNameString

------------------------------------------------------------------------------
-- | Like 'bvar', but works over standard GHC 'OccName's.
bvar' :: BVar a => OccName -> a
bvar' = bvar . fromString . occNameString


------------------------------------------------------------------------------
-- | Use something in the hypothesis to fill the hole.
assumption :: TacticsM ()
assumption = rule $ \(Judgement hy g) ->
  case find ((== g) . snd) $ toList hy of
    Just (v, _) -> pure $ noLoc $ var' v
    Nothing -> throwError $ GoalMismatch "assumption" g


------------------------------------------------------------------------------
-- | Introduce a lambda.
intro :: TacticsM ()
intro = rule $ \(Judgement hy g) ->
  case splitFunTy_maybe $ unCType g of
    Just (a, b) -> do
      v <- pure $ mkGoodName (getInScope hy) a
      sg <- newSubgoal (M.singleton v (CType a) <> hy) $ CType b
      pure $ noLoc $ lambda [bvar' v] $ unLoc sg
    _ -> throwError $ GoalMismatch "intro" g

------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros :: TacticsM ()
intros = rule $ \(Judgement hy g) ->
  case tcSplitFunTys $ unCType g of
    ([], _) -> throwError $ GoalMismatch "intro" g
    (as, b) -> do
      vs <- mkManyGoodNames hy as
      sg <- newSubgoal (M.fromList (zip vs $ fmap CType as) <> hy) $ CType b
      pure
        . noLoc
        . lambda (fmap bvar' vs)
        $ unLoc sg


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destruct' :: (DataCon -> Judgement -> Rule) -> OccName -> TacticsM ()
destruct' f term = rule $ \(Judgement hy g) -> do
  case find ((== term) . fst) $ toList hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (_, t) ->
      case splitTyConApp_maybe $ unCType t of
        Nothing -> throwError $ GoalMismatch "destruct" g
        Just (tc, apps) -> do
          fmap noLoc
              $ case' (var' term)
              <$> do
            for (tyConDataCons tc) $ \dc -> do
              let args = dataConInstArgTys dc apps
              names <- mkManyGoodNames hy args

              let pat :: Pat GhcPs
                  pat = conP (fromString $ occNameString $ nameOccName $ dataConName dc)
                      $ fmap bvar' names

              j <- newJudgement (M.fromList (zip names (fmap CType args)) <> hy) g
              sg <- f dc j
              pure $ match [pat] $ unLoc sg


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destruct :: OccName -> TacticsM ()
destruct = destruct' $ const subgoal


------------------------------------------------------------------------------
-- | Case split, using the same data constructor in the matches.
homo :: OccName -> TacticsM ()
homo = destruct' $ \dc (Judgement hy (CType g)) ->
  buildDataCon hy dc (snd $ splitAppTys g)


------------------------------------------------------------------------------
-- | Ensure a tactic produces no subgoals. Useful when working with
-- backtracking.
solve :: TacticsM () -> TacticsM ()
solve t = t >> throwError NoProgress

------------------------------------------------------------------------------
-- | Apply a function from the hypothesis.
apply :: TacticsM ()
apply = rule $ \jdg@(Judgement hy g) -> do
  case find ((== Just g) . fmap (CType . snd) . splitFunTy_maybe . unCType . snd) $ toList hy of
    Just (func, ty) -> applySpecific func ty jdg
    Nothing -> throwError $ GoalMismatch "apply" g

apply' :: OccName -> TacticsM ()
apply' func = rule $ \(Judgement hys g) ->
    case M.lookup func hys of
      Just (CType ty) -> do
          let (args, ret) = splitFunTys ty
          unify g (CType ret)
          sgs <- traverse (newSubgoal hys . CType) args
          pure . noLoc
               . foldl' (@@) (var' func)
               $ fmap unLoc sgs
      Nothing -> throwError $ GoalMismatch "apply" g

applySpecific :: OccName -> CType -> Judgement -> RuleM (LHsExpr GhcPs)
applySpecific func (CType ty) (Judgement hy _) = do
  let (args, _) = splitFunTys ty
  sgs <- traverse (newSubgoal hy . CType) args
  pure . noLoc
       . foldl' (@@) (var' func)
       $ fmap unLoc sgs


instantiate :: OccName -> CType -> TacticsM ()
instantiate func (CType ty) = rule $ \jdg@(Judgement _ (CType g)) -> do
  -- TODO(sandy): We need to get available from the type sig and compare
  -- against _ctx
  let (_binders, _ctx, tcSplitFunTys -> (_, res)) = tcSplitSigmaTy ty
  unify (CType res) (CType g)
  applySpecific func (CType ty) jdg
  -- case oneWayUnify binders res g of
  --   Just subst ->
  --     -- TODO(sandy): How does this affect the judgment wrt our new ununified
  --     -- tyvars?
  --     let (_fresh_vars, ty') = instantiateType $ substTy subst ty
  --      in applySpecific func (CType ty') jdg
    -- Nothing -> throwError $ GoalMismatch  "instantiate" $ CType g


------------------------------------------------------------------------------
-- | Introduce a data constructor, splitting a goal into the datacon's
-- constituent sub-goals.
split :: TacticsM ()
split = rule $ \(Judgement hy g) ->
  case splitTyConApp_maybe $ unCType g of
    Just (tc, apps) ->
      case tyConDataCons tc of
        [dc] -> buildDataCon hy dc apps
        _ -> throwError $ GoalMismatch "split" g
    Nothing -> throwError $ GoalMismatch "split" g


------------------------------------------------------------------------------
-- | Run 'one' many times.
deepen :: Int -> TacticsM ()
deepen 0 = pure ()
deepen depth = do
  one
  deepen $ depth - 1

------------------------------------------------------------------------------
-- | @matching f@ takes a function from a judgement to a @Tactic@, and
-- then applies the resulting @Tactic@.
matching :: (Judgement -> TacticsM ()) -> TacticsM ()
matching f = TacticT $ StateT $ \s -> runStateT (unTacticT $ f $ s) s

attemptOn :: (a -> TacticsM ()) -> (Judgement -> [a]) -> TacticsM ()
attemptOn tac getNames = matching (choice . fmap (\s -> tac s) . getNames)

------------------------------------------------------------------------------
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = TacticT $ StateT $ \(Judgement _ goal) -> runStateT (unTacticT $ auto' 5) (Judgement M.empty goal)

auto' :: Int -> TacticsM ()
auto' 0 = throwError NoProgress
auto' n = do
    intros <|> many_ intro
    choice
           [ attemptOn (\fname -> apply' fname >> (auto' $ n - 1)) functionNames
           , attemptOn (\aname -> progress ((==) `on` jGoal) NoProgress (destruct aname) >> (auto' $ n - 1)) algebraicNames
           , split >> (auto' $ n - 1)
           , assumption >> (auto' $ n - 1)
           ]
  where
    functionNames :: Judgement -> [OccName]
    functionNames (Judgement hys _) = M.keys $ M.filter (isFunction . unCType) hys

    algebraicNames :: Judgement -> [OccName]
    algebraicNames (Judgement hys _) = M.keys $ M.filter (isJust . algebraicTyCon . unCType) hys

------------------------------------------------------------------------------
-- | Run a tactic, and subsequently apply auto if it completes. If not, just
-- run the first tactic, leaving its subgoals as holes.
autoIfPossible :: TacticsM () -> TacticsM ()
autoIfPossible t = (t >> solve auto) <|> t


------------------------------------------------------------------------------
-- | Do one obvious thing.
one :: TacticsM ()
one = intro <|> assumption <|> split <|> apply <|> pure ()

