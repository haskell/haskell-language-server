{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Tactic.Tactics
  ( module Ide.Plugin.Tactic.Tactics
  , runTactic
  ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.Reader.Class (MonadReader(ask))
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (StateT(..), runStateT)
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
assumption :: TacticsM ()
assumption = attemptOn allNames assume


------------------------------------------------------------------------------
-- | Use something named in the hypothesis to fill the hole.
assume :: OccName -> TacticsM ()
assume name = rule $ \jdg -> do
  let g  = jGoal jdg
  case M.lookup name $ jHypothesis jdg of
    Just ty ->
      case ty == jGoal jdg of
        True  -> do
          case M.member name (jPatHypothesis jdg) of
            True  -> setRecursionFrameData True
            False -> pure ()
          useOccName jdg name
          pure $ noLoc $ var' name
        False -> throwError $ GoalMismatch "assume" g
    Nothing -> throwError $ UndefinedHypothesis name



recursion :: TacticsM ()
recursion = do
  defs <- getCurrentDefinitions
  attemptOn (const $ fmap fst defs) $ \name -> do
    modify $ withRecursionStack (False :)
    filterT recursiveCleanup (withRecursionStack tail) $ do
      (localTactic (apply' (const id) name) $ introducing defs)
        <@> fmap (localTactic assumption . filterPosition name) [0..]


------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
--
-- TODO(sandy): THIS THING IS A BIG BIG HACK
--
-- Why? Two reasons. It uses extremelyStupid__definingFunction, which is stupid
-- in and of itself (see the note there.) Additionally, this doesn't check to
-- make sure we're in the top-level scope, so it will set the recursive
-- position mapping any time 'intros' is called.
intros :: TacticsM ()
intros = rule $ \jdg -> do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  ctx <- ask
  case tcSplitFunTys $ unCType g of
    ([], _) -> throwError $ GoalMismatch "intro" g
    (as, b) -> do
      vs <- mkManyGoodNames hy as
      let jdg' = withPositionMapping (extremelyStupid__definingFunction ctx) vs
               $ introducing (zip vs $ coerce as)
               $ withNewGoal (CType b) jdg
      modify $ withIntroducedVals $ mappend $ S.fromList vs
      sg <- newSubgoal jdg'
      pure
        . noLoc
        . lambda (fmap bvar' vs)
        $ unLoc sg


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destructAuto :: OccName -> TacticsM ()
destructAuto name = do
  jdg <- goal
  case hasDestructed jdg name of
    True -> throwError $ AlreadyDestructed name
    False ->
      let subtactic = rule $ destruct' (const subgoal) name
       in case isPatVal jdg name of
            True ->
              pruning subtactic $ \jdgs ->
                let getHyTypes = S.fromList . fmap snd . M.toList . jHypothesis
                    new_hy = foldMap getHyTypes jdgs
                    old_hy = getHyTypes jdg
                 in case S.null $ new_hy S.\\ old_hy of
                      True  -> Just $ UnhelpfulDestruct name
                      False -> Nothing
            False -> subtactic

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


apply :: OccName -> TacticsM ()
apply = apply' (const id)


apply' :: (Int -> Judgement -> Judgement) -> OccName -> TacticsM ()
apply' f func = do
  rule $ \jdg -> do
    let hy = jHypothesis jdg
        g  = jGoal jdg
    case M.lookup func hy of
      Just (CType ty) -> do
          let (args, ret) = splitFunTys ty
          unify g (CType ret)
          useOccName jdg func
          sgs <- traverse ( \(i, t) ->
                            newSubgoal
                          . f i
                          . blacklistingDestruct
                          . flip withNewGoal jdg
                          $ CType t
                          ) $ zip [0..] args
          pure . noLoc
              . foldl' (@@) (var' func)
              $ fmap unLoc sgs
      Nothing -> do
        throwError $ GoalMismatch "apply" g


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


localTactic :: TacticsM a -> (Judgement -> Judgement) -> TacticsM a
localTactic t f = do
  TacticT $ StateT $ \jdg ->
    runStateT (unTacticT t) $ f jdg


------------------------------------------------------------------------------
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = do
  current <- getCurrentDefinitions
  jdg <- goal
  traceM $ mappend "!!!auto current:" $ show current
  traceM $ mappend "!!!auto jdg:" $ show jdg
  localTactic (auto' 4) $ disallowing $ fmap fst current


auto' :: Int -> TacticsM ()
auto' 0 = throwError NoProgress
auto' n = do
  let loop = auto' (n - 1)
  try intros
  choice
    [ attemptOn functionNames $ \fname -> do
        apply fname
        loop
    , attemptOn algebraicNames $ \aname -> do
        destructAuto aname
        loop
    , split >> loop
    , assumption >> loop
    , recursion
    ]


functionNames :: Judgement -> [OccName]
functionNames  =
  M.keys . M.filter (isFunction . unCType) . jHypothesis


algebraicNames :: Judgement -> [OccName]
algebraicNames =
  M.keys . M.filter (isJust . algebraicTyCon . unCType) . jHypothesis


allNames :: Judgement -> [OccName]
allNames = M.keys . jHypothesis

