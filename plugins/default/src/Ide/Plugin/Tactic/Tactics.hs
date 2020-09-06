{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Tactic.Tactics
  ( module Ide.Plugin.Tactic.Tactics
  , runTactic
  ) where

import           Control.Monad.Except (throwError)
import           Data.List
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
import           TcType
import           TyCoRep
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
  case unCType g of
    (FunTy a b) -> do
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
solve :: TacticsM () -> TacticsM  ()
solve t = t >> throwError NoProgress


------------------------------------------------------------------------------
-- | Apply a function from the hypothesis.
apply :: TacticsM ()
apply = rule $ \(Judgement hy g) -> do
  case find ((== Just g) . fmap (CType . snd) . splitFunTy_maybe . unCType . snd) $ toList hy of
    Just (func, CType ty) -> do
      let (args, _) = splitFunTys ty
      sgs <- traverse (newSubgoal hy . CType) args
      pure . noLoc
           . foldl' (@@) (var' func)
           $ fmap unLoc sgs
    Nothing -> throwError $ GoalMismatch "apply" g


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
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = (intro >> auto)
   <!> (assumption >> auto)
   <!> (split >> auto)
   <!> (apply >> auto)
   <!> pure ()


------------------------------------------------------------------------------
-- | Run a tactic, and subsequently apply auto if it completes. If not, just
-- run the first tactic, leaving its subgoals as holes.
autoIfPossible :: TacticsM () -> TacticsM ()
autoIfPossible t = (t >> solve auto) <!> t


------------------------------------------------------------------------------
-- | Do one obvious thing.
one :: TacticsM ()
one = intro <!> assumption <!> split <!> apply <!> pure ()

