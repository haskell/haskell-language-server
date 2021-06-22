{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Wingman.Machinery where

import           Control.Applicative (empty)
import           Control.Lens ((<>~))
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.State.Class (gets, modify, MonadState)
import           Control.Monad.State.Strict (StateT (..), execStateT)
import           Control.Monad.Trans.Maybe
import           Data.Coerce
import           Data.Either
import           Data.Foldable
import           Data.Functor ((<&>))
import           Data.Generics (everything, gcount, mkQ)
import           Data.Generics.Product (field')
import           Data.List (sortBy)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.Monoid (getSum)
import           Data.Ord (Down (..), comparing)
import qualified Data.Set as S
import           Data.Traversable (for)
import           Development.IDE.Core.Compile (lookupName)
import           Development.IDE.GHC.Compat
import           GhcPlugins (GlobalRdrElt (gre_name), lookupOccEnv, varType)
import           Refinery.ProofState
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           TcType
import           Type (tyCoVarsOfTypeWellScoped)
import           Wingman.Context (getInstance)
import           Wingman.GHC (tryUnifyUnivarsButNotSkolems, updateSubst, tacticsGetDataCons)
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
  ctx <- ask
  unifier <- getSubstForJudgement j
  subgoal
    $ normalizeJudgement ctx
    $ substJdg unifier
    $ unsetIsTopHole
    $ normalizeJudgement ctx j


tacticToRule :: Judgement -> TacticsM () -> Rule
tacticToRule jdg (TacticT tt) = RuleT $ flip execStateT jdg tt >>= flip Subgoal Axiom


------------------------------------------------------------------------------
-- | Attempt to generate a term of the right type using in-scope bindings, and
-- a given tactic.
runTactic
    :: Context
    -> Judgement
    -> TacticsM ()       -- ^ Tactic to use
    -> IO (Either [TacticError] RunTacticResults)
runTactic ctx jdg t = do
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
    res <- flip runReaderT ctx
         . unExtractM
         $ runTacticT t jdg tacticState
    pure $ case partitionEithers res of
      (errs, []) -> Left $ take 50 errs
      (_, fmap assoc23 -> solns) -> do
        let sorted =
              flip sortBy solns $ comparing $ \(ext, (_, holes)) ->
                Down $ scoreSolution ext jdg holes
        case sorted of
          ((syn, (_, subgoals)) : _) ->
            Right $
              RunTacticResults
                { rtr_trace    = syn_trace syn
                , rtr_extract  = simplify $ syn_val syn
                , rtr_subgoals = subgoals
                , rtr_other_solns = reverse . fmap fst $ sorted
                , rtr_jdg = jdg
                , rtr_ctx = ctx
                }
          -- guaranteed to not be empty
          _ -> Left []

assoc23 :: (a, b, c) -> (a, (b, c))
assoc23 (a, b, c) = (a, (b, c))


tracePrim :: String -> Trace
tracePrim = flip rose []


------------------------------------------------------------------------------
-- | Mark that a tactic used the given string in its extract derivation. Mainly
-- used for debugging the search when things go terribly wrong.
tracing
    :: Functor m
    => String
    -> TacticT jdg (Synthesized ext) err s m a
    -> TacticT jdg (Synthesized ext) err s m a
tracing s = mappingExtract (mapTrace $ rose s . pure)


------------------------------------------------------------------------------
-- | Mark that a tactic performed recursion. Doing so incurs a small penalty in
-- the score.
markRecursion
    :: Functor m
    => TacticT jdg (Synthesized ext) err s m a
    -> TacticT jdg (Synthesized ext) err s m a
markRecursion = mappingExtract (field' @"syn_recursion_count" <>~ 1)


------------------------------------------------------------------------------
-- | Map a function over the extract created by a tactic.
mappingExtract
    :: Functor m
    => (ext -> ext)
    -> TacticT jdg ext err s m a
    -> TacticT jdg ext err s m a
mappingExtract f (TacticT m)
  = TacticT $ StateT $ \jdg ->
      mapExtract' f $ runStateT m jdg


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
       , Reward Bool   -- all bindings used
       , Penalize Int  -- unused top-level bindings
       , Penalize Int  -- number of introduced bindings
       , Reward Int    -- number used bindings
       , Penalize Int  -- number of recursive calls
       , Penalize Int  -- size of extract
       )
scoreSolution ext goal holes
  = ( Penalize $ length holes
    , Reward   $ S.null $ intro_vals S.\\ used_vals
    , Penalize $ S.size unused_top_vals
    , Penalize $ S.size intro_vals
    , Reward   $ S.size used_vals + length used_user_vals
    , Penalize $ getSum $ syn_recursion_count ext
    , Penalize $ solutionSize $ syn_val ext
    )
  where
    initial_scope = hyByName $ jEntireHypothesis goal
    intro_vals = M.keysSet $ hyByName $ syn_scoped ext
    used_vals = S.intersection intro_vals $ syn_used_vals ext
    used_user_vals = filter (isLocalHypothesis . hi_provenance)
                   $ mapMaybe (flip M.lookup initial_scope)
                   $ S.toList
                   $ syn_used_vals ext
    top_vals = S.fromList
             . fmap hi_name
             . filter (isTopLevel . hi_provenance)
             . unHypothesis
             $ syn_scoped ext
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
-- | Attempt to unify two types.
unify :: CType -- ^ The goal type
      -> CType -- ^ The type we are trying unify the goal type with
      -> RuleM ()
unify goal inst = do
  skolems <- gets ts_skolems
  case tryUnifyUnivarsButNotSkolems skolems goal inst of
    Just subst ->
      modify $ updateSubst subst
    Nothing -> throwError (UnificationError inst goal)


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
-- | Mystical time-traveling combinator for inspecting the extracts produced by
-- a tactic. We can use it to guard that extracts match certain predicates, for
-- example.
--
-- Note, that this thing is WEIRD. To illustrate:
--
-- @@
-- peek f
-- blah
-- @@
--
-- Here, @f@ can inspect the extract _produced by @blah@,_  which means the
-- causality appears to go backwards.
--
-- 'peek' should be exposed directly by @refinery@ in the next release.
peek :: (ext -> TacticT jdg ext err s m ()) -> TacticT jdg ext err s m ()
peek k = tactic $ \j -> Subgoal ((), j) $ \e -> proofState (k e) j


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
    Nothing -> throwError $ NotInScope name

------------------------------------------------------------------------------
-- | Lift a function over 'HyInfo's to one that takes an 'OccName' and tries to
-- look it up in the hypothesis.
useNameFromContext :: (HyInfo CType -> TacticsM a) -> OccName -> TacticsM a
useNameFromContext f name = do
  lookupNameInContext name >>= \case
    Just ty -> f $ createImportedHyInfo name ty
    Nothing -> throwError $ NotInScope name


------------------------------------------------------------------------------
-- | Find the type of an 'OccName' that is defined in the current module.
lookupNameInContext :: MonadReader Context m => OccName -> m (Maybe CType)
lookupNameInContext name = do
  ctx <- asks ctxModuleFuncs
  pure $ case find ((== name) . fst) ctx of
    Just (_, ty) -> pure ty
    Nothing      -> empty


getDefiningType
    :: (MonadError TacticError m, MonadReader Context m)
    => m CType
getDefiningType = do
  calling_fun_name <- fst . head <$> asks ctxDefiningFuncs
  maybe
    (throwError $ NotInScope calling_fun_name)
    pure
      =<< lookupNameInContext calling_fun_name


------------------------------------------------------------------------------
-- | Build a 'HyInfo' for an imported term.
createImportedHyInfo :: OccName -> CType -> HyInfo CType
createImportedHyInfo on ty = HyInfo
  { hi_name = on
  , hi_provenance = ImportPrv
  , hi_type = ty
  }


getTyThing
    :: OccName
    -> TacticsM (Maybe TyThing)
getTyThing occ = do
  ctx <- ask
  case lookupOccEnv (ctx_occEnv ctx) occ of
    Just (elt : _) -> do
      mvar <- lift
            $ ExtractM
            $ lift
            $ lookupName (ctx_hscEnv ctx) (ctx_module ctx)
            $ gre_name elt
      pure mvar
    _ -> pure Nothing


------------------------------------------------------------------------------
-- | Like 'getTyThing' but specialized to classes.
knownClass :: OccName -> TacticsM (Maybe Class)
knownClass occ =
  getTyThing occ <&> \case
    Just (ATyCon tc) -> tyConClass_maybe tc
    _                -> Nothing


------------------------------------------------------------------------------
-- | Like 'getInstance', but uses a class that it just looked up.
getKnownInstance :: OccName -> [Type] -> TacticsM (Maybe (Class, PredType))
getKnownInstance f tys = runMaybeT $ do
  cls <- MaybeT $ knownClass f
  MaybeT $ getInstance cls tys


------------------------------------------------------------------------------
-- | Lookup the type of any 'OccName' that was imported. Necessarily done in
-- IO, so we only expose this functionality to the parser. Internal Haskell
-- code that wants to lookup terms should do it via 'KnownThings'.
getOccNameType
    :: OccName
    -> TacticsM Type
getOccNameType occ = do
  getTyThing occ >>= \case
    Just (AnId v) -> pure $ varType v
    _ -> throwError $ NotInScope occ


getCurrentDefinitions :: TacticsM [(OccName, CType)]
getCurrentDefinitions = do
  ctx_funcs <- asks ctxDefiningFuncs
  for ctx_funcs $ \res@(occ, _) ->
    pure . maybe res (occ,) =<< lookupNameInContext occ


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

