{-# LANGUAGE CPP           #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Wingman.Machinery where

import           Control.Concurrent.Chan.Unagi.NoBlocking (newChan, writeChan, OutChan, tryRead, tryReadChan)
import           Control.Monad.Reader
import           Control.Monad.State.Class (gets, modify, MonadState)
import           Control.Monad.State.Strict (StateT (..), execStateT)
import           Data.Coerce
import           Data.Foldable
import           Data.Generics (everything, gcount, mkQ)
import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Maybe (isNothing)
import           Data.Ord (Down (..), comparing)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat hiding (isTopLevel, empty)
import           Refinery.Future
import           Refinery.ProofState
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           System.Timeout (timeout)
import           Wingman.GHC (tryUnifyUnivarsButNotSkolems, updateSubst, tacticsGetDataCons, freshTyvars)
import           Wingman.Judgements
import           Wingman.Simplify (simplify)
import           Wingman.Types


substCTy :: TCvSubst -> CType -> CType
substCTy subst = coerce . substTy subst . coerce


getSubstForJudgement
    :: MonadState TacticState m
    => Judgement
    -> m TCvSubst
getSubstForJudgement j = do
  -- NOTE(sandy): It's OK to use mempty here, because coercions _can_ give us
  -- substitutions for skolems.
  let coercions = j_coercion j
  unifier <- gets ts_unifier
  pure $ unionTCvSubst unifier coercions

------------------------------------------------------------------------------
-- | Produce a subgoal that must be solved before we can solve the original
-- goal.
newSubgoal
    :: Judgement
    -> Rule
newSubgoal j = do
  unifier <- getSubstForJudgement j
  subgoal
    $ substJdg unifier
    $ unsetIsTopHole j


tacticToRule :: Judgement -> TacticsM () -> Rule
tacticToRule jdg (TacticT tt) = RuleT $ execStateT tt jdg >>= flip Subgoal Axiom


consumeChan :: OutChan (Maybe a) -> IO [a]
consumeChan chan = do
  tryReadChan chan >>= tryRead >>= \case
    Nothing -> pure []
    Just (Just a) -> (a:) <$> consumeChan chan
    Just Nothing -> pure []


------------------------------------------------------------------------------
-- | Attempt to generate a term of the right type using in-scope bindings, and
-- a given tactic.
runTactic
    :: Int          -- ^ Timeout
    -> Config
    -> Judgement
    -> TacticsM ()  -- ^ Tactic to use
    -> IO (Either [TacticError] RunTacticResults)
runTactic duration ctx jdg t = do
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

    let stream = hoistListT (flip runReaderT ctx . unExtractM)
               $ runStreamingTacticT t jdg tacticState
    (in_proofs, out_proofs) <- newChan
    (in_errs, out_errs) <- newChan
    timed_out <-
      fmap isNothing $ timeout duration $ consume stream $ \case
        Left err -> writeChan in_errs $ Just err
        Right proof -> writeChan in_proofs $ Just proof
    writeChan in_proofs Nothing

    solns <- consumeChan out_proofs
    let sorted =
          flip sortBy solns $ comparing $ \(Proof ext _ holes) ->
            Down $ scoreSolution ext jdg $ fmap snd holes
    case sorted of
      ((Proof syn _ _) : _) ->
        pure $ Right $
          RunTacticResults
            { rtr_extract  = simplify $ syn_val syn
            , rtr_jdg = jdg
            , rtr_ctx = ctx
            , rtr_timed_out = timed_out
            }
      _ -> fmap Left $ consumeChan out_errs


------------------------------------------------------------------------------
-- | Map a function over the extract created by a tactic.
mappingExtract
    :: Functor m
    => (ext -> ext)
    -> TacticT jdg ext err s m a
    -> TacticT jdg ext err s m a
mappingExtract f (TacticT m)
  = TacticT $ StateT $ \jdg ->
      mapExtract id f $ runStateT m jdg


------------------------------------------------------------------------------
-- | Given the results of running a tactic, score the solutions by
-- desirability.
--
-- NOTE: This function is completely unprincipled and was just hacked together
-- to produce the right test results.
scoreSolution
    :: Synthesized (LHsExpr GhcPs)
    -> Judgement
    -> [Judgement]
    -> ( Penalize Int  -- number of holes
       , Penalize Int  -- size of extract
       )
scoreSolution ext _ holes
  = ( Penalize $ length holes
    , Penalize $ solutionSize $ syn_val ext
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
-- | Generate a unique unification variable.
newUnivar :: MonadState TacticState m => m Type
newUnivar = do
  freshTyvars $
    mkInfForAllTys [alphaTyVar] alphaTy


------------------------------------------------------------------------------
-- | Attempt to unify two types.
unify :: CType -- ^ The goal type
      -> CType -- ^ The type we are trying unify the goal type with
      -> RuleM ()
unify goal inst = do
  skolems <- gets ts_skolems
  case tryUnifyUnivarsButNotSkolems skolems goal inst of
    Just subst ->
      modify $ updateSubst subst
    Nothing -> cut


cut :: RuleT jdg ext err s m a
cut = RuleT Empty


------------------------------------------------------------------------------
-- | Attempt to unify two types.
canUnify
    :: MonadState TacticState m
    => CType -- ^ The goal type
    -> CType -- ^ The type we are trying unify the goal type with
    -> m Bool
canUnify goal inst = do
  skolems <- gets ts_skolems
  case tryUnifyUnivarsButNotSkolems skolems goal inst of
    Just _ -> pure True
    Nothing -> pure False


------------------------------------------------------------------------------
-- | Prefer the first tactic to the second, if the bool is true. Otherwise, just run the second tactic.
--
-- This is useful when you have a clever pruning solution that isn't always
-- applicable.
attemptWhen :: TacticsM a -> TacticsM a -> Bool -> TacticsM a
attemptWhen _  t2 False = t2
attemptWhen t1 t2 True  = commit t1 t2


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
    _ -> failure TooPolymorphic


------------------------------------------------------------------------------
-- | The 'try' that comes in refinery 0.3 causes unnecessary backtracking and
-- balloons the search space. This thing just tries it, but doesn't backtrack
-- if it fails.
--
-- NOTE(sandy): But there's a bug! Or at least, something not understood here.
-- Using this everywhere breaks te tests, and neither I nor TOTBWF are sure
-- why.  Prefer 'try' if you can, and only try this as a last resort.
--
-- TODO(sandy): Remove this when we upgrade to 0.4
try'
    :: Functor m
    => TacticT jdg ext err s m ()
    -> TacticT jdg ext err s m ()
try' t = commit t $ pure ()


------------------------------------------------------------------------------
-- | Sorry leaves a hole in its extract
exact :: HsExpr GhcPs -> TacticsM ()
exact = rule . const . pure . pure . noLoc

------------------------------------------------------------------------------
-- | Lift a function over 'HyInfo's to one that takes an 'OccName' and tries to
-- look it up in the hypothesis.
useNameFromHypothesis :: (HyInfo CType -> TacticsM a) -> OccName -> TacticsM a
useNameFromHypothesis f name = do
  hy <- jHypothesis <$> goal
  case M.lookup name $ hyByName hy of
    Just hi -> f hi
    Nothing -> failure $ NotInScope name



------------------------------------------------------------------------------
-- | Given two types, see if we can construct a homomorphism by mapping every
-- data constructor in the domain to the same in the codomain. This function
-- returns 'Just' when all the lookups succeeded, and a non-empty value if the
-- homomorphism *is not* possible.
uncoveredDataCons :: Type -> Type -> Maybe (S.Set (Uniquely DataCon))
uncoveredDataCons domain codomain = do
  (g_dcs, _) <- tacticsGetDataCons codomain
  (hi_dcs, _) <- tacticsGetDataCons domain
  pure $ S.fromList (coerce hi_dcs) S.\\ S.fromList (coerce g_dcs)

