{-# LANGUAGE TupleSections #-}
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

import           Control.Applicative (Alternative((<|>)))
import           Control.Arrow
import           Control.Monad (when)
import           Control.Monad.Except (throwError)
import           Control.Monad.Extra (unlessM)
import           Control.Monad.Reader.Class (MonadReader(ask))
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (execStateT, StateT(..), runStateT)
import           Data.Bool (bool)
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           DataCon
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
import           Name (occNameString)
import           PrelNames (applicativeClassName)
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
    Just ty -> do
      unify ty $ jGoal jdg
      when (M.member name $ jPatHypothesis jdg) $
        setRecursionFrameData True
      useOccName jdg name
      pure $ (tracePrim $ "assume " <> occNameString name, ) $ noLoc $ var' name
    Nothing -> throwError $ UndefinedHypothesis name


recursion :: TacticsM ()
recursion = requireConcreteHole $ tracing "recursion" $ do
  defs <- getCurrentDefinitions
  attemptOn (const $ fmap fst defs) $ \name -> do
    modify $ withRecursionStack (False :)
    ensure recursiveCleanup (withRecursionStack tail) $ do
      (localTactic (apply name) $ introducing defs)
        <@> fmap (localTactic assumption . filterPosition name) [0..]


------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros :: TacticsM ()
intros = rule $ \jdg -> do
  let hy = jHypothesis jdg
      g  = jGoal jdg
  ctx <- ask
  case tcSplitFunTys $ unCType g of
    ([], _) -> throwError $ GoalMismatch "intros" g
    (as, b) -> do
      vs <- mkManyGoodNames hy as
      let jdg' = introducing (zip vs $ coerce as)
               $ withNewGoal (CType b) jdg
      modify $ withIntroducedVals $ mappend $ S.fromList vs
      (tr, sg)
        <- newSubgoal
          $ bool
              id
              (withPositionMapping
                (extremelyStupid__definingFunction ctx) vs)
              (isTopHole jdg)
          $ jdg'
      pure
        . (rose ("intros {" <> intercalate ", " (fmap show vs) <> "}") $ pure tr, )
        . noLoc
        . lambda (fmap bvar' vs)
        $ unLoc sg


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destructAuto :: OccName -> TacticsM ()
destructAuto name = requireConcreteHole $ tracing "destruct(auto)" $ do
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
destruct name = requireConcreteHole $ tracing "destruct(user)" $ do
  jdg <- goal
  case hasDestructed jdg name of
    True -> throwError $ AlreadyDestructed name
    False -> rule $ \jdg -> destruct' (const subgoal) name jdg


------------------------------------------------------------------------------
-- | Case split, using the same data constructor in the matches.
homo :: OccName -> TacticsM ()
homo = requireConcreteHole . tracing "homo" . rule . destruct' (\dc jdg ->
  buildDataCon jdg dc $ snd $ splitAppTys $ unCType $ jGoal jdg)


------------------------------------------------------------------------------
-- | LambdaCase split, and leave holes in the matches.
destructLambdaCase :: TacticsM ()
destructLambdaCase = tracing "destructLambdaCase" $ rule $ destructLambdaCase' (const subgoal)


------------------------------------------------------------------------------
-- | LambdaCase split, using the same data constructor in the matches.
homoLambdaCase :: TacticsM ()
homoLambdaCase =
  tracing "homoLambdaCase" $
    rule $ destructLambdaCase' $ \dc jdg ->
      buildDataCon jdg dc
        . snd
        . splitAppTys
        . unCType
        $ jGoal jdg


apply :: OccName -> TacticsM ()
apply func = requireConcreteHole $ tracing ("apply' " <> show func) $ do
  jdg <- goal
  let hy = jHypothesis jdg
      g  = jGoal jdg
  case M.lookup func hy of
    Just (CType ty) -> do
      ty' <- freshTyvars ty
      let (_, _, args, ret) = tacticsSplitFunTy ty'
      requireNewHoles $ rule $ \jdg -> do
        unify g (CType ret)
        useOccName jdg func
        (tr, sgs)
            <- fmap unzipTrace
            $ traverse ( newSubgoal
                        . blacklistingDestruct
                        . flip withNewGoal jdg
                        . CType
                        ) args
        pure
          . (tr, )
          . noLoc
          . foldl' (@@) (var' func)
          $ fmap unLoc sgs
    Nothing -> do
      throwError $ GoalMismatch "apply" g


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors.
split :: TacticsM ()
split = tracing "split(user)" $ do
  jdg <- goal
  let g = jGoal jdg
  case splitTyConApp_maybe $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (tc, _) -> do
      let dcs = tyConDataCons tc
      choice $ fmap splitDataCon dcs


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors. Different than
-- 'split' because it won't split a data con if it doesn't result in any new
-- goals.
splitAuto :: TacticsM ()
splitAuto = requireConcreteHole $ tracing "split(auto)" $ do
  jdg <- goal
  let g = jGoal jdg
  case splitTyConApp_maybe $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (tc, _) -> do
      let dcs = tyConDataCons tc
      case isSplitWhitelisted jdg of
        True -> choice $ fmap splitDataCon dcs
        False -> do
          choice $ flip fmap dcs $ \dc -> requireNewHoles $
            splitDataCon dc


------------------------------------------------------------------------------
-- | Allow the given tactic to proceed if and only if it introduces holes that
-- have a different goal than current goal.
requireNewHoles :: TacticsM () -> TacticsM ()
requireNewHoles m = do
  jdg <- goal
  pruning m $ \jdgs ->
    case null jdgs || any (/= jGoal jdg) (fmap jGoal jdgs) of
      True  -> Nothing
      False -> Just NoProgress


------------------------------------------------------------------------------
-- | Attempt to instantiate the given data constructor to solve the goal.
splitDataCon :: DataCon -> TacticsM ()
splitDataCon dc =
  requireConcreteHole $ tracing ("splitDataCon:" <> show dc) $ rule $ \jdg -> do
    let g = jGoal jdg
    case splitTyConApp_maybe $ unCType g of
      Just (tc, apps) -> do
        case elem dc $ tyConDataCons tc of
          True -> buildDataCon (unwhitelistingSplit jdg) dc apps
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
-- | Attempt to run the given tactic in "idiom bracket" mode. For example, if
-- the current goal is
--
--    (_ :: [r])
--
-- then @idiom apply@ will remove the applicative context, resulting in a hole:
--
--    (_ :: r)
--
-- and then use @apply@ to solve it. Let's say this results in:
--
--    (f (_ :: a) (_ :: b))
--
-- Finally, @idiom@ lifts this back into the original applicative:
--
--    (f <$> (_ :: [a]) <*> (_ :: [b]))
--
-- Idiom will fail fast if the current goal doesn't have an applicative
-- instance.
idiom :: TacticsM () -> TacticsM ()
idiom m = do -- disallowWhenDeriving (S.fromList ["fmap", "<*>", "liftA2"]) $ do
  jdg <- goal
  case splitAppTy_maybe $ unCType $ jGoal jdg of
    Just (applic, ty) -> do
      -- unlessM (hasInstance applicativeClassName [applic]) $
      --   throwError $ GoalMismatch "idiom" $ CType applic
      rule $ \jdg -> do
        (tr, expr) <- subgoalWith (withNewGoal (CType ty) jdg) m
        case unLoc expr of
          HsApp{} -> pure (tr, idiomize expr)
          _       -> throwError $ GoalMismatch "idiom" $ jGoal jdg
      rule $ newSubgoal . withModifiedGoal (CType . mkAppTy applic . unCType)
    Nothing -> throwError $ GoalMismatch "idiom" $ jGoal jdg


subgoalWith :: Judgement -> TacticsM () -> RuleM (Trace, LHsExpr GhcPs)
subgoalWith jdg t = RuleT $ flip execStateT jdg $ unTacticT t


auto' :: Int -> TacticsM ()
auto' 0 = throwError NoProgress
auto' n = do
  let loop = auto' (n - 1)
  try intros
  choice
    [ overFunctions $ \fname -> do
        idiom (apply fname) <|> apply fname
        loop
    , overAlgebraicTerms $ \aname -> do
        destructAuto aname
        loop
    , do
        idiom splitAuto <|> splitAuto
        loop
    , assumption >> loop
    , recursion
    ]

overFunctions :: (OccName -> TacticsM ()) -> TacticsM ()
overFunctions =
  attemptOn $ M.keys . M.filter (isFunction . unCType) . jHypothesis

overAlgebraicTerms :: (OccName -> TacticsM ()) -> TacticsM ()
overAlgebraicTerms =
  attemptOn $
    M.keys . M.filter (isJust . algebraicTyCon . unCType) . jHypothesis


allNames :: Judgement -> [OccName]
allNames = M.keys . jHypothesis

