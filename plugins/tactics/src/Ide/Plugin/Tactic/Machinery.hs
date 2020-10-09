{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
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
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.State (MonadState(..))
import           Control.Monad.State.Class (gets, modify)
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
    subgoal
      $ substJdg unifier
      $ unsetIsTopHole j


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
        tacticState = defaultTacticState { ts_skolems = skolems }
    in case partitionEithers
          . flip runReader ctx
          . unExtractM
          $ runTacticTWithState t jdg tacticState of
      (errs, []) -> Left $ take 50 $ errs
      (_, solns) -> do
        let sorted = sortBy (comparing $ Down . uncurry scoreSolution . snd) solns
        -- TODO(sandy): remove this trace sometime
        traceM $ mappend "!!!solns: " $ intercalate "\n" $ take 50 $  fmap (show . fst) sorted
        case sorted of
          (res : _) -> Right $ fst res
          -- guaranteed to not be empty
          _ -> Left []


recursiveCleanup
    :: TacticState
    -> Maybe TacticError
recursiveCleanup s =
  let r = head $ ts_recursion_stack s
   in case r of
        True  -> Nothing
        False -> Just NoProgress


filterT
    :: (Monad m)
    => (s -> Maybe err)
    -> (s -> s)
    -> TacticT jdg ext err s m ()
    -> TacticT jdg ext err s m ()
filterT p f t = check >> t
    where
      check = rule $ \j -> do
          e <- subgoal j
          s <- get
          modify f
          case p s of
            Just err -> throwError err
            Nothing -> pure e


gather
    :: (MonadExtract ext m)
    => TacticT jdg ext err s m a
    -> ([(a, jdg)] -> TacticT jdg ext err s m a)
    -> TacticT jdg ext err s m a
gather t f = tactic $ \j -> do
    s <- get
    results <- lift $ proofs s $ proofState t j
    msum $ flip fmap results $ \case
        Left err -> throwError err
        Right (_, jdgs) -> proofState (f jdgs) j


pruning
    :: (MonadExtract ext m)
    => TacticT jdg ext err s m ()
    -> ([jdg] -> Maybe err)
    -> TacticT jdg ext err s m ()
pruning t p = gather t $ maybe t throwError . p . fmap snd


setRecursionFrameData :: MonadState TacticState m => Bool -> m ()
setRecursionFrameData b = do
  modify $ withRecursionStack $ \case
    (_ : bs) -> b : bs
    []       -> []


scoreSolution
    :: TacticState
    -> [Judgement]
    -> ( Penalize Int  -- number of holes
       , Reward Bool   -- all bindings used
       , Penalize Int  -- number of introduced bindings
       , Reward Int    -- number used bindings
       )
scoreSolution TacticState{..} holes
  = ( Penalize $ length holes
    , Reward $ S.null $ ts_intro_vals S.\\ ts_used_vals
    , Penalize $ S.size ts_intro_vals
    , Reward $ S.size ts_used_vals
    )


newtype Penalize a = Penalize a
  deriving (Eq, Ord, Show) via (Down a)

newtype Reward a = Reward a
  deriving (Eq, Ord, Show) via a


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
      go s goals (Commit p1 p2) = go s goals p1 >>= \case
          (rights -> []) -> go s goals p2
          solns -> pure solns
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

