{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Tactic.Tactics
  ( module Ide.Plugin.Tactic.Tactics
  , runTactic
  ) where

import           Control.Applicative
import           Control.Monad.Except (throwError)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (StateT(..), runStateT)
import           Data.Function
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
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
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           TcType
import           Type hiding (Var)



------------------------------------------------------------------------------
-- | Use something in the hypothesis to fill the hole.
-- TODO(sandy): deprecate this
assumption :: TacticsM ()
assumption = rule $ \jdg -> do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case find ((== g) . snd) $ toList hy of
    Just (v, _) -> pure $ noLoc $ var' v
    Nothing -> throwError $ GoalMismatch "assumption" g


------------------------------------------------------------------------------
-- | Use something named in the hypothesis to fill the hole.
assume :: OccName -> TacticsM ()
assume name = rule $ \jdg -> do
  let g  = jGoal jdg
  case M.lookup name $ jHypothesis jdg of
    Just ty ->
      case ty == jGoal jdg of
        True  -> do
          useOccName jdg name
          pure $ noLoc $ var' name
        False -> throwError $ GoalMismatch "assume" g
    Nothing -> throwError $ UndefinedHypothesis name


useOccName :: MonadState TacticState m => Judgement -> OccName -> m ()
useOccName jdg name =
  case M.lookup name $ jHypothesis jdg of
    Just{}  -> modify $ withUsedVals $ S.insert name
    Nothing -> pure ()


------------------------------------------------------------------------------
-- | Introduce a lambda.
intro :: TacticsM ()
intro = rule $ \jdg -> do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case splitFunTy_maybe $ unCType g of
    Just (a, b) -> do
      v <- pure $ mkGoodName (getInScope hy) a
      let jdg' = introducing [(v, CType a)] $ withNewGoal (CType b) jdg
      sg <- newSubgoal jdg'
      pure $ noLoc $ lambda [bvar' v] $ unLoc sg
    _ -> throwError $ GoalMismatch "intro" g


------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros :: TacticsM ()
intros = rule $ \jdg -> do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case tcSplitFunTys $ unCType g of
    ([], _) -> throwError $ GoalMismatch "intro" g
    (as, b) -> do
      vs <- mkManyGoodNames hy as
      let jdg' = introducing (zip vs $ coerce as) $ withNewGoal (CType b) jdg
      sg <- newSubgoal jdg'
      pure
        . noLoc
        . lambda (fmap bvar' vs)
        $ unLoc sg


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destruct :: OccName -> TacticsM ()
destruct name = do
  jdg <- goal
  case hasDestructed jdg name of
    True -> throwError $ AlreadyDestructed name
    False -> rule $ \jdg -> destruct' (const subgoal) name jdg


------------------------------------------------------------------------------
-- | Case split, using the same data constructor in the matches.
homo :: OccName -> TacticsM ()
homo = rule . destruct' (\dc jdg ->
  buildDataCon jdg dc $ snd $ splitAppTys $ unCType $ jGoal jdg)


------------------------------------------------------------------------------
-- | LambdaCase split, and leave holes in the matches.
destructLambdaCase :: TacticsM ()
destructLambdaCase = rule $ destructLambdaCase' (const subgoal)


------------------------------------------------------------------------------
-- | LambdaCase split, using the same data constructor in the matches.
homoLambdaCase :: TacticsM ()
homoLambdaCase = rule $ destructLambdaCase' (\dc jdg ->
  buildDataCon jdg dc $ snd $ splitAppTys $ unCType $ jGoal jdg)


apply' :: OccName -> TacticsM ()
apply' func = rule $ \jdg -> do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case M.lookup func hy of
    Just (CType ty) -> do
        let (args, ret) = splitFunTys ty
        unify g (CType ret)
        sgs <- traverse (newSubgoal . flip withNewGoal jdg . CType) args
        pure . noLoc
             . foldl' (@@) (var' func)
             $ fmap unLoc sgs
    Nothing -> throwError $ GoalMismatch "apply" g


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors.
split :: TacticsM ()
split = do
  jdg <- goal
  let g = jGoal jdg
  case splitTyConApp_maybe $ unCType g of
    Nothing -> throwError $ GoalMismatch "getGoalTyCon" g
    Just (tc, _) -> do
      let dcs = tyConDataCons tc
      choice $ fmap splitDataCon dcs


------------------------------------------------------------------------------
-- | Attempt to instantiate the given data constructor to solve the goal.
splitDataCon :: DataCon -> TacticsM ()
splitDataCon dc = rule $ \jdg -> do
  let g = jGoal jdg
  case splitTyConApp_maybe $ unCType g of
    Just (tc, apps) -> do
      case elem dc $ tyConDataCons tc of
        True -> buildDataCon jdg dc apps
        False -> throwError $ IncorrectDataCon dc
    Nothing -> throwError $ GoalMismatch "splitDataCon" g


------------------------------------------------------------------------------
-- | @matching f@ takes a function from a judgement to a @Tactic@, and
-- then applies the resulting @Tactic@.
matching :: (Judgement -> TacticsM ()) -> TacticsM ()
matching f = TacticT $ StateT $ \s -> runStateT (unTacticT $ f s) s


attemptOn :: (Judgement -> [a]) -> (a -> TacticsM ()) -> TacticsM ()
attemptOn getNames tac = matching (choice . fmap (\s -> tac s) . getNames)


------------------------------------------------------------------------------
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = do
  current <- getCurrentDefinitions
  TacticT $ StateT $ \jdg ->
    runStateT (unTacticT $ auto' 5) $ disallowing current jdg

auto' :: Int -> TacticsM ()
auto' 0 = throwError NoProgress
auto' n = do
  let loop = auto' (n - 1)
  intros <|> many_ intro
  choice
    [ attemptOn functionNames $ \fname -> do
        apply' fname
        loop
    , attemptOn algebraicNames $ \aname -> do
        progress ((==) `on` jGoal) NoProgress (destruct aname)
        loop
    , split >> loop
    , attemptOn allNames $ \name -> do
        assume name
        loop
    ]


functionNames :: Judgement -> [OccName]
functionNames  =
  M.keys . M.filter (isFunction . unCType) . jHypothesis


algebraicNames :: Judgement -> [OccName]
algebraicNames =
  M.keys . M.filter (isJust . algebraicTyCon . unCType) . jHypothesis


allNames :: Judgement -> [OccName]
allNames = M.keys . jHypothesis

