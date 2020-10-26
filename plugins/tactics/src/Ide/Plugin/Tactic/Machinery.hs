{-# LANGUAGE ScopedTypeVariables #-}
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

import           Class (Class(classTyVars))
import           Control.Arrow
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.State (MonadState(..))
import           Control.Monad.State.Class (gets, modify)
import           Control.Monad.State.Strict (StateT (..))
import           Data.Coerce
import           Data.Either
import           Data.Functor ((<&>))
import           Data.Generics (mkQ, everything, gcount)
import           Data.List (sortBy)
import           Data.Ord (comparing, Down(..))
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           HscTypes (lookupTypeEnv)
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Types
import           InstEnv (emptyInstEnv, lookupInstEnv, InstEnvs(InstEnvs))
import           Module (emptyModuleSet)
import           OccName (mkVarOcc, HasOccName(occName))
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
              flip sortBy solns $ comparing $ \((_, ext), (jdg, holes)) ->
                Down $ scoreSolution ext jdg holes
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
    :: LHsExpr GhcPs
    -> TacticState
    -> [Judgement]
    -> ( Penalize Int  -- number of holes
       , Reward Bool   -- all bindings used
       , Penalize Int  -- number of introduced bindings
       , Reward Int    -- number used bindings
       , Penalize Int  -- size of extract
       )
scoreSolution ext TacticState{..} holes
  = ( Penalize $ length holes
    , Reward   $ S.null $ ts_intro_vals S.\\ ts_used_vals
    , Penalize $ S.size ts_intro_vals
    , Reward   $ S.size ts_used_vals
    , Penalize $ solutionSize ext
    )


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
tryUnifyUnivarsButNotSkolems :: [TyVar] -> CType -> CType -> Maybe TCvSubst
tryUnifyUnivarsButNotSkolems skolems goal inst =
  case tcUnifyTysFG (skolemsOf skolems) [unCType inst] [unCType goal] of
    Unifiable subst -> pure subst
    _ -> Nothing


------------------------------------------------------------------------------
-- | Helper method for 'tryUnifyUnivarsButNotSkolems'
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


------------------------------------------------------------------------------
-- | Get the class methods of a 'PredType', correctly dealing with
-- instantiation of quantified class types.
methodHypothesis :: PredType -> Maybe [(OccName, CType)]
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
    in (occName method,  CType $ substTy subst ty)


------------------------------------------------------------------------------
-- | Lookup the given class.
lookupClass :: MonadReader Context m => Name -> m (Maybe Class)
lookupClass nm = do
  tenv <- asks ctxTypeEnv
  pure $ case lookupTypeEnv tenv nm of
    Just (ATyCon tc) -> tyConClass_maybe tc
    Just x -> traceX "found some class" (unsafeRender x) Nothing
    Nothing          -> trace "NOTHING" Nothing


------------------------------------------------------------------------------
-- | Check if the types have a MPTC instance for the given clas name.;
hasInstance :: MonadReader Context m => Name -> [Type] -> m Bool
hasInstance nm tys = do
  insts <- asks ctxInstEnv
  lookupClass nm >>= \case
    Just cls ->
      case lookupInstEnv False (InstEnvs insts emptyInstEnv emptyModuleSet) cls tys of
        (matches, _, _) -> pure $ not $ null matches
    Nothing  -> error $ "hasInstance: CANT FIND CLASS " <> show (occName nm)


--------------------------------------------------------------------------------
-- | Run the given tactic iff the current hole contains no univars. Skolems and
-- already decided univars are OK though.
requireConcreteHole :: TacticsM a -> TacticsM a
requireConcreteHole m = do
  jdg     <- goal
  skolems <- gets $ S.fromList . ts_skolems
  let vars = S.fromList $ tyCoVarsOfTypeWellScoped $ unCType $ jGoal jdg
  case S.size $ vars S.\\ skolems of
    0 -> m
    _ -> throwError TooPolymorphic


------------------------------------------------------------------------------
-- | Prevent the tactic from running when deriving a function with a name in
-- the given set. Useful for preventing bottoms.
disallowWhenDeriving
    :: Set String
    -> TacticsM a
    -> TacticsM a
disallowWhenDeriving what m = do
  defs <- asks $ S.fromList . fmap fst . ctxDefiningFuncs
  case S.null $ defs S.\\ S.map mkVarOcc what of
    True  -> m
    False -> throwError NoProgress

