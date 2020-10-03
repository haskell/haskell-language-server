{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Ide.Plugin.Tactic.Machinery
  ( module Ide.Plugin.Tactic.Machinery
  ) where

import           Control.Applicative
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader
import           Control.Monad.State (gets, modify)
import           Data.Coerce
import           Data.Either
import           Data.List (intercalate, sortBy)
import           Data.Ord (comparing, Down(..))
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Types
import           Refinery.ProofState
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           TcType
import           Type
import           Unify


substCTy :: TCvSubst -> CType -> CType
substCTy subst = coerce . substTy subst . coerce


------------------------------------------------------------------------------
-- | Produce a subgoal that must be solved before we can solve the original
-- goal.
newSubgoal
    :: Judgement
    -> RuleM (LHsExpr GhcPs)
newSubgoal j = do
    unifier <- gets ts_unifier
    subgoal $ substJdg unifier j


------------------------------------------------------------------------------
-- | Attempt to generate a term of the right type using in-scope bindings, and
-- a given tactic.
runTactic
    :: Context
    -> Judgement
    -> TacticsM ()       -- ^ Tactic to use
    -> Either [TacticError] (LHsExpr GhcPs)
runTactic ctx jdg t =
    let skolems = tyCoVarsOfTypeWellScoped $ unCType $ jGoal jdg
        tacticState = mempty { ts_skolems = skolems }
    in case partitionEithers
          . flip runReader ctx
          . unExtractM
          $ runTacticTWithState t jdg tacticState of
      (errs, []) -> Left $ errs
      (_, solns) -> do
        -- TODO(sandy): remove this trace sometime
        traceM $ intercalate "\n" $ fmap (unsafeRender . fst) $ solns
        case sortBy (comparing $ Down . uncurry scoreSolution . snd) solns of
          (res : _) -> Right $ fst res
          -- guaranteed to not be empty
          _ -> Left []


scoreSolution :: TacticState -> [Judgement] -> Int
scoreSolution TacticState{..} holes
  -- TODO(sandy): should this be linear?
  = S.size ts_used_vals - length holes * 5


runTacticTWithState
    :: (MonadExtract ext m)
    => TacticT jdg ext err s m ()
    -> jdg
    -> s
    -> m [Either err (ext, (s, [jdg]))]
runTacticTWithState t j s = proofs' s $ fmap snd $ proofState t j


proofs'
    :: (MonadExtract ext m)
    => s
    -> ProofStateT ext ext err s m goal
    -> m [(Either err (ext, (s, [goal])))]
proofs' s p = go s [] p
    where
      go s goals (Subgoal goal k) = do
         h <- hole
         (go s (goals ++ [goal]) $ k h)
      go s goals (Effect m) = go s goals =<< m
      go s goals (Stateful f) =
          let (s', p) = f s
          in go s' goals p
      go s goals (Alt p1 p2) = liftA2 (<>) (go s goals p1) (go s goals p2)
      go s goals (Interleave p1 p2) = liftA2 (interleave) (go s goals p1) (go s goals p2)
      go _ _ Empty = pure []
      go _ _ (Failure err) = pure [throwError err]
      go s goals (Axiom ext) = pure [Right (ext, (s, goals))]


------------------------------------------------------------------------------
-- | We need to make sure that we don't try to unify any skolems.
-- To see why, consider the case:
--
-- uhh :: (Int -> Int) -> a
-- uhh f = _
--
-- If we were to apply 'f', then we would try to unify 'Int' and 'a'.
-- This is fine from the perspective of 'tcUnifyTy', but will cause obvious
-- type errors in our use case. Therefore, we need to ensure that our
-- 'TCvSubst' doesn't try to unify skolems.
checkSkolemUnification :: CType -> CType -> TCvSubst -> RuleM ()
checkSkolemUnification t1 t2 subst = do
    skolems <- gets ts_skolems
    unless (all (flip notElemTCvSubst subst) skolems) $
      throwError (UnificationError t1 t2)


------------------------------------------------------------------------------
-- | Attempt to unify two types.
unify :: CType -- ^ The goal type
      -> CType -- ^ The type we are trying unify the goal type with
      -> RuleM ()
unify goal inst =
    case tcUnifyTy (unCType inst) (unCType goal) of
      Just subst -> do
          checkSkolemUnification inst goal subst
          modify (\s -> s { ts_unifier = unionTCvSubst subst (ts_unifier s) })
      Nothing -> throwError (UnificationError inst goal)

