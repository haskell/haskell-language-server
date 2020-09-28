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
  , module Ide.Plugin.Tactic.Debug
  , module Ide.Plugin.Tactic.Types
  , module Ide.Plugin.Tactic.Judgements
  , module Ide.Plugin.Tactic.Context
  , module Ide.Plugin.Tactic.Range
  ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.Reader
import           Control.Monad.State (gets, get, modify, evalStateT)
import           Data.Char
import           Data.Coerce
import           Data.Either
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.LocalBindings
import           Development.IDE.Types.Location
import           Ide.Plugin.Tactic.Context
import           Ide.Plugin.Tactic.Debug
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Range
import           Ide.Plugin.Tactic.Types

import           GHC.SourceGen.Overloaded
import           Name
import           Refinery.Tactic
import           TcType
import           Type
import           TysWiredIn (listTyCon, pairTyCon, intTyCon, floatTyCon, doubleTyCon, charTyCon)
import           Unify


------------------------------------------------------------------------------
-- | Orphan instance for producing holes when attempting to solve tactics.
instance MonadExtract (LHsExpr GhcPs) ProvableM where
  hole = pure $ noLoc $ HsVar noExtField $ noLoc $ Unqual $ mkVarOcc "_"

substCTy :: TCvSubst -> CType -> CType
substCTy subst = coerce . substTy subst . coerce

------------------------------------------------------------------------------
-- | Given a 'SrcSpan' and a 'Bindings', create a hypothesis.
hypothesisFromBindings :: RealSrcSpan -> Bindings -> Map OccName CType
hypothesisFromBindings span bs = buildHypothesis $ getLocalScope bs span


------------------------------------------------------------------------------
-- | Convert a @Set Id@ into a hypothesis.
buildHypothesis :: [(Name, Maybe Type)] -> Map OccName CType
buildHypothesis
  = M.fromList
  . mapMaybe go
  where
    go (occName -> occ, t)
      | Just ty <- t
      , isAlpha . head . occNameString $ occ = Just (occ, CType ty)
      | otherwise = Nothing


------------------------------------------------------------------------------
-- | Produce a subgoal that must be solved before we can solve the original
-- goal.
newSubgoal
    :: Map OccName CType  -- ^ Available bindings
    -> CType              -- ^ Sub-goal type
    -> RuleM (LHsExpr GhcPs)
newSubgoal hy g = do
    j <- newJudgement hy g
    unifier <- gets ts_unifier
    subgoal $ substJdg unifier j

------------------------------------------------------------------------------
-- | Create a new judgment
newJudgement
    ::  Monad m
    => Map OccName CType  -- ^ Available bindings
    -> CType              -- ^ Sub-goal type
    -> m Judgement
newJudgement hy g = do
  pure $ Judgement hy mempty g


------------------------------------------------------------------------------
-- | Produce a unique, good name for a type.
mkGoodName
    :: [OccName]  -- ^ Bindings in scope; used to ensure we don't shadow anything
    -> Type       -- ^ The type to produce a name for
    -> OccName
mkGoodName in_scope t =
  let tn = mkTyName t
   in mkVarOcc $ case elem (mkVarOcc tn) in_scope of
        True -> tn ++ show (length in_scope)
        False -> tn


------------------------------------------------------------------------------
-- | Like 'mkGoodName' but creates several apart names.
mkManyGoodNames
  :: (Traversable t, Monad m)
  => M.Map OccName a
  -> t Type
  -> m (t OccName)
mkManyGoodNames hy args =
  flip evalStateT (getInScope hy) $ for args $ \at -> do
    in_scope <- Control.Monad.State.get
    let n = mkGoodName in_scope at
    modify (n :)
    pure n


------------------------------------------------------------------------------
-- | Use type information to create a reasonable name.
mkTyName :: Type -> String
-- eg. mkTyName (a -> B) = "fab"
mkTyName (tcSplitFunTys -> ([a@(isFunTy -> False)], b))
  = "f" ++ mkTyName a ++ mkTyName b
-- eg. mkTyName (a -> b -> C) = "f_C"
mkTyName (tcSplitFunTys -> ((_:_), b))
  = "f_" ++ mkTyName b
-- eg. mkTyName (Either A B) = "eab"
mkTyName (splitTyConApp_maybe -> Just (c, args))
  = mkTyConName c ++ foldMap mkTyName args
-- eg. mkTyName a = "a"
mkTyName (getTyVar_maybe-> Just tv)
  = occNameString $ occName tv
-- eg. mkTyName (forall x. y) = "y"
mkTyName (tcSplitSigmaTy-> ((_:_), _, t))
  = mkTyName t
mkTyName _ = "x"


------------------------------------------------------------------------------
-- | Is this a function type?
isFunction :: Type -> Bool
isFunction (tcSplitFunTys -> ((_:_), _)) = True
isFunction _ = False

------------------------------------------------------------------------------
-- | Is this an algebraic type?
algebraicTyCon :: Type -> Maybe TyCon
algebraicTyCon (splitTyConApp_maybe -> Just (tycon, _))
  | tycon == intTyCon    = Nothing
  | tycon == floatTyCon  = Nothing
  | tycon == doubleTyCon = Nothing
  | tycon == charTyCon   = Nothing
  | tycon == funTyCon    = Nothing
  | otherwise = Just tycon
algebraicTyCon _ = Nothing


------------------------------------------------------------------------------
-- | Get a good name for a type constructor.
mkTyConName :: TyCon -> String
mkTyConName tc
  | tc == listTyCon = "l_"
  | tc == pairTyCon = "p_"
  | otherwise = fmap toLower . take 1 . occNameString $ getOccName tc


------------------------------------------------------------------------------
-- | Attempt to generate a term of the right type using in-scope bindings, and
-- a given tactic.
runTactic
    :: Context
    -> Judgement
    -> TacticsM ()       -- ^ Tactic to use
    -> Either [TacticError] (LHsExpr GhcPs)
runTactic ctx jdg t =
    -- FIXME [Reed] This code does not work
    let skolems = tyCoVarsOfTypeWellScoped $ unCType $ jGoal jdg
        tacticState = mempty { ts_skolems = skolems }
    in case partitionEithers $ flip runReader ctx $ runProvableT $ runTacticT t jdg tacticState of
      (errs, []) -> Left $ errs
      (_, solns) ->
        let soln = listToMaybe $ filter (null . snd) solns
        in Right $ fst $ fromMaybe (head solns) soln


------------------------------------------------------------------------------
-- | Which names are in scope?
getInScope :: Map OccName a -> [OccName]
getInScope = M.keys


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Map OccName CType  -- ^ In-scope bindings
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (LHsExpr GhcPs)
buildDataCon hy dc apps = do
  let args = dataConInstArgTys dc apps
  sgs <- traverse (newSubgoal hy . CType) args
  pure
    . noLoc
    . foldl' (@@)
        (HsVar noExtField $ noLoc $ Unqual $ nameOccName $ dataConName dc)
    $ fmap unLoc sgs


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

