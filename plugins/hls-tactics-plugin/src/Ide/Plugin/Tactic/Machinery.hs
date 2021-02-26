{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Ide.Plugin.Tactic.Machinery
  ( module Ide.Plugin.Tactic.Machinery
  ) where

import           Class (Class(classTyVars))
import           Control.Arrow
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.State (MonadState(..))
import           Control.Monad.State.Class (gets, modify)
import           Control.Monad.State.Strict (StateT (..))
import           Data.Bool (bool)
import           Data.Coerce
import           Data.Either
import           Data.Foldable
import           Data.Functor ((<&>))
import           Data.Generics (mkQ, everything, gcount)
import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Ord (comparing, Down(..))
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Simplify (simplify)
import           Ide.Plugin.Tactic.Types
import           OccName (HasOccName(occName))
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
    let skolems = S.fromList
                $ foldMap (tyCoVarsOfTypeWellScoped . unCType)
                $ (:) (jGoal jdg)
                $ fmap hi_type
                $ toList
                $ hyByName
                $ jHypothesis jdg
        tacticState =
          defaultTacticState
            { ts_skolems = skolems
            }
    in case partitionEithers
          . flip runReader ctx
          . unExtractM
          $ runTacticT t jdg tacticState of
      (errs, []) -> Left $ take 50 errs
      (_, fmap assoc23 -> solns) -> do
        let sorted =
              flip sortBy solns $ comparing $ \(ext, (jdg, holes)) ->
                Down $ scoreSolution ext jdg holes
        case sorted of
          ((syn, _) : _) ->
            Right $
              RunTacticResults
                { rtr_trace = syn_trace syn
                , rtr_extract = simplify $ syn_val syn
                , rtr_other_solns = reverse . fmap fst $ take 5 sorted
                , rtr_jdg = jdg
                , rtr_ctx = ctx
                }
          -- guaranteed to not be empty
          _ -> Left []

assoc23 :: (a, b, c) -> (a, (b, c))
assoc23 (a, b, c) = (a, (b, c))


tracePrim :: String -> Trace
tracePrim = flip rose []


tracing
    :: Functor m
    => String
    -> TacticT jdg (Synthesized ext) err s m a
    -> TacticT jdg (Synthesized ext) err s m a
tracing s (TacticT m)
  = TacticT $ StateT $ \jdg ->
      mapExtract' (mapTrace $ rose s . pure) $ runStateT m jdg


------------------------------------------------------------------------------
-- | Recursion is allowed only when we can prove it is on a structurally
-- smaller argument. The top of the 'ts_recursion_stack' witnesses the smaller
-- pattern val.
guardStructurallySmallerRecursion
    :: TacticState
    -> Maybe TacticError
guardStructurallySmallerRecursion s =
  case head $ ts_recursion_stack s of
     Just _  -> Nothing
     Nothing -> Just NoProgress


------------------------------------------------------------------------------
-- | Mark that the current recursive call is structurally smaller, due to
-- having been matched on a pattern value.
--
-- Implemented by setting the top of the 'ts_recursion_stack'.
markStructuralySmallerRecursion :: MonadState TacticState m => PatVal -> m ()
markStructuralySmallerRecursion pv = do
  modify $ withRecursionStack $ \case
    (_ : bs) -> Just pv : bs
    []       -> []


------------------------------------------------------------------------------
-- | Given the results of running a tactic, score the solutions by
-- desirability.
--
-- NOTE: This function is completely unprincipled and was just hacked together
-- to produce the right test results.
scoreSolution
    :: Synthesized (LHsExpr GhcPs)
    -> TacticState
    -> [Judgement]
    -> ( Penalize Int  -- number of holes
       , Reward Bool   -- all bindings used
       , Penalize Int  -- unused top-level bindings
       , Penalize Int  -- number of introduced bindings
       , Reward Int    -- number used bindings
       , Penalize Int  -- number of recursive calls
       , Penalize Int  -- size of extract
       )
scoreSolution ext TacticState{..} holes
  = ( Penalize $ length holes
    , Reward   $ S.null $ intro_vals S.\\ used_vals
    , Penalize $ S.size unused_top_vals
    , Penalize $ S.size intro_vals
    , Reward   $ S.size used_vals
    , Penalize ts_recursion_count
    , Penalize $ solutionSize $ syn_val ext
    )
  where
    intro_vals = M.keysSet $ hyByName $ syn_scoped ext
    used_vals = S.intersection intro_vals $ syn_used_vals ext
    top_vals = S.fromList . fmap hi_name . filter (isTopLevel . hi_provenance) $ unHypothesis $ syn_scoped ext
    unused_top_vals = top_vals S.\\ used_vals


------------------------------------------------------------------------------
-- | Compute the number of 'LHsExpr' nodes; used as a rough metric for code
-- size.
solutionSize :: LHsExpr GhcPs -> Int
solutionSize = everything (+) $ gcount $ mkQ False $ \case
  (_ :: LHsExpr GhcPs) -> True


newtype Penalize a = Penalize a
  deriving (Eq, Ord, Show) via (Down a)

newtype Reward a = Reward a
  deriving (Eq, Ord, Show) via a


------------------------------------------------------------------------------
-- | Like 'tcUnifyTy', but takes a list of skolems to prevent unification of.
tryUnifyUnivarsButNotSkolems :: Set TyVar -> CType -> CType -> Maybe TCvSubst
tryUnifyUnivarsButNotSkolems skolems goal inst =
  case tcUnifyTysFG
         (bool BindMe Skolem . flip S.member skolems)
         [unCType inst]
         [unCType goal] of
    Unifiable subst -> pure subst
    _ -> Nothing



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


------------------------------------------------------------------------------
-- | Get the class methods of a 'PredType', correctly dealing with
-- instantiation of quantified class types.
methodHypothesis :: PredType -> Maybe [HyInfo CType]
methodHypothesis ty = do
  (tc, apps) <- splitTyConApp_maybe ty
  cls <- tyConClass_maybe tc
  let methods = classMethods cls
      tvs     = classTyVars cls
      subst   = zipTvSubst tvs apps
  sc_methods <- fmap join
              $ traverse (methodHypothesis . substTy subst)
              $ classSCTheta cls
  pure $ mappend sc_methods $ methods <&> \method ->
    let (_, _, ty) = tcSplitSigmaTy $ idType method
    in ( HyInfo (occName method) (ClassMethodPrv $ Uniquely cls) $ CType $ substTy subst ty
       )


------------------------------------------------------------------------------
-- | Run the given tactic iff the current hole contains no univars. Skolems and
-- already decided univars are OK though.
requireConcreteHole :: TacticsM a -> TacticsM a
requireConcreteHole m = do
  jdg     <- goal
  skolems <- gets ts_skolems
  let vars = S.fromList $ tyCoVarsOfTypeWellScoped $ unCType $ jGoal jdg
  case S.size $ vars S.\\ skolems of
    0 -> m
    _ -> throwError TooPolymorphic
