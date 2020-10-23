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

import           Control.Arrow
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.State (MonadState(..))
import           Control.Monad.State.Class (gets, modify)
import           Control.Monad.State.Strict (StateT (..))
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
    -> Rule
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
    -> Either [TacticError] RunTacticResults
runTactic ctx jdg t =
    let skolems = tyCoVarsOfTypeWellScoped $ unCType $ jGoal jdg
        tacticState = defaultTacticState { ts_skolems = skolems }
    in case partitionEithers
          . flip runReader ctx
          . unExtractM
          $ runTacticT t jdg tacticState of
      (errs, []) -> Left $ take 50 $ errs
      (_, fmap assoc23 -> solns) -> do
        let sorted =
              sortBy (comparing $ Down . uncurry scoreSolution . snd) solns
        case sorted of
          (((tr, ext), _) : _) ->
            Right
              . RunTacticResults tr ext
              . reverse
              . fmap fst
              $ take 5 sorted
          -- guaranteed to not be empty
          _ -> Left []

assoc23 :: (a, b, c) -> (a, (b, c))
assoc23 (a, b, c) = (a, (b, c))


tracePrim :: String -> Trace
tracePrim = flip rose []


tracing
    :: Functor m
    => String
    -> TacticT jdg (Trace, ext) err s m a
    -> TacticT jdg (Trace, ext) err s m a
tracing s (TacticT m)
  = TacticT $ StateT $ \jdg ->
      mapExtract' (first $ rose s . pure) $ runStateT m jdg


recursiveCleanup
    :: TacticState
    -> Maybe TacticError
recursiveCleanup s =
  let r = head $ ts_recursion_stack s
   in case r of
        True  -> Nothing
        False -> Just NoProgress


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



tryUnifyUnivarsButNotSkolems :: [TyVar] -> CType -> CType -> Maybe TCvSubst
tryUnifyUnivarsButNotSkolems skolems goal inst =
  case tcUnifyTysFG (skolemsOf skolems) [unCType inst] [unCType goal] of
    Unifiable subst -> pure subst
    _ -> Nothing


skolemsOf :: [TyVar] -> TyVar -> BindFlag
skolemsOf tvs tv =
  case elem tv tvs of
    True  -> Skolem
    False -> BindMe


------------------------------------------------------------------------------
-- | Attempt to unify two types.
unify :: CType -- ^ The goal type
      -> CType -- ^ The type we are trying unify the goal type with
      -> RuleM ()
unify goal inst = do
  skolems <- gets ts_skolems
  case tryUnifyUnivarsButNotSkolems skolems goal inst of
    Just subst ->
      modify (\s -> s { ts_unifier = unionTCvSubst subst (ts_unifier s) })
    Nothing -> throwError (UnificationError inst goal)

