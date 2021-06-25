{-# LANGUAGE OverloadedStrings #-}

module Wingman.Tactics
  ( module Wingman.Tactics
  , runTactic
  ) where

import           ConLike (ConLike(RealDataCon))
import           Control.Applicative (Alternative(empty))
import           Control.Lens ((&), (%~), (<>~))
import           Control.Monad (filterM)
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Control.Monad.Extra (anyM)
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Control.Monad.State.Strict (StateT(..), runStateT)
import           Data.Bool (bool)
import           Data.Foldable
import           Data.Functor ((<&>))
import           Data.Generics.Labels ()
import           Data.List
import           Data.List.Extra (dropEnd, takeEnd)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen ((@@))
import           GHC.SourceGen.Expr
import           Name (occNameString, occName)
import           OccName (mkVarOcc)
import           Refinery.Tactic
import           Refinery.Tactic.Internal
import           TcType
import           Type hiding (Var)
import           TysPrim (betaTy, alphaTy, betaTyVar, alphaTyVar)
import           Wingman.CodeGen
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Machinery
import           Wingman.Naming
import           Wingman.StaticPlugin (pattern MetaprogramSyntax)
import           Wingman.Types


------------------------------------------------------------------------------
-- | Use something in the hypothesis to fill the hole.
assumption :: TacticsM ()
assumption = attemptOn (S.toList . allNames) assume


------------------------------------------------------------------------------
-- | Use something named in the hypothesis to fill the hole.
assume :: OccName -> TacticsM ()
assume name = rule $ \jdg -> do
  case M.lookup name $ hyByName $ jHypothesis jdg of
    Just (hi_type -> ty) -> do
      unify ty $ jGoal jdg
      pure $
        -- This slightly terrible construct is producing a mostly-empty
        -- 'Synthesized'; but there is no monoid instance to do something more
        -- reasonable for a default value.
        (pure (noLoc $ var' name))
          { syn_trace = tracePrim $ "assume " <> occNameString name
          , syn_used_vals = S.singleton name
          }
    Nothing -> throwError $ UndefinedHypothesis name


------------------------------------------------------------------------------
-- | Like 'apply', but uses an 'OccName' available in the context
-- or the module
use :: Saturation -> OccName -> TacticsM ()
use sat occ = do
  ctx <- ask
  ty <- case lookupNameInContext occ ctx of
    Just ty -> pure ty
    Nothing -> CType <$> getOccNameType occ
  apply sat $ createImportedHyInfo occ ty


recursion :: TacticsM ()
-- TODO(sandy): This tactic doesn't fire for the @AutoThetaFix@ golden test,
-- presumably due to running afoul of 'requireConcreteHole'. Look into this!
recursion = requireConcreteHole $ tracing "recursion" $ do
  defs <- getCurrentDefinitions
  attemptOn (const defs) $ \(name, ty) -> markRecursion $ do
    -- Peek allows us to look at the extract produced by this block.
    peek $ \ext -> do
      jdg <- goal
      let pat_vals = jPatHypothesis jdg
      -- Make sure that the recursive call contains at least one already-bound
      -- pattern value. This ensures it is structurally smaller, and thus
      -- suggests termination.
      unless (any (flip M.member pat_vals) $ syn_used_vals ext) empty

    let hy' = recursiveHypothesis defs
    ctx <- ask
    localTactic (apply Saturated $ HyInfo name RecursivePrv ty) (introduce ctx hy')
      <@> fmap (localTactic assumption . filterPosition name) [0..]


restrictPositionForApplication :: TacticsM () -> TacticsM () -> TacticsM ()
restrictPositionForApplication f app = do
  -- NOTE(sandy): Safe use of head; context is guaranteed to have a defining
  -- binding
  name <- head . fmap fst <$> getCurrentDefinitions
  f <@>
    fmap
      (localTactic app . filterPosition name) [0..]


------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros :: TacticsM ()
intros = intros' Nothing

------------------------------------------------------------------------------
-- | Introduce a lambda binding every variable.
intros'
    :: Maybe [OccName]  -- ^ When 'Nothing', generate a new name for every
                        -- variable. Otherwise, only bind the variables named.
    -> TacticsM ()
intros' names = rule $ \jdg -> do
  let g  = jGoal jdg
  case tacticsSplitFunTy $ unCType g of
    (_, _, [], _) -> throwError $ GoalMismatch "intros" g
    (_, _, args, res) -> do
      ctx <- ask
      let occs = fromMaybe (mkManyGoodNames (hyNamesInScope $ jEntireHypothesis jdg) args) names
          num_occs = length occs
          top_hole = isTopHole ctx jdg
          bindings = zip occs $ coerce args
          bound_occs = fmap fst bindings
          hy' = lambdaHypothesis top_hole bindings
          jdg' = introduce ctx hy'
               $ withNewGoal (CType $ mkFunTys' (drop num_occs args) res) jdg
      ext <- newSubgoal jdg'
      pure $
        ext
          & #syn_trace %~ rose ("intros {" <> intercalate ", " (fmap show bound_occs) <> "}")
                        . pure
          & #syn_scoped <>~ hy'
          & #syn_val   %~ noLoc . lambda (fmap bvar' bound_occs) . unLoc


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destructAuto :: HyInfo CType -> TacticsM ()
destructAuto hi = requireConcreteHole $ tracing "destruct(auto)" $ do
  jdg <- goal
  let subtactic = destructOrHomoAuto hi
  case isPatternMatch $ hi_provenance hi of
    True ->
      pruning subtactic $ \jdgs ->
        let getHyTypes = S.fromList . fmap hi_type . unHypothesis . jHypothesis
            new_hy = foldMap getHyTypes jdgs
            old_hy = getHyTypes jdg
        in case S.null $ new_hy S.\\ old_hy of
              True  -> Just $ UnhelpfulDestruct $ hi_name hi
              False -> Nothing
    False -> subtactic


------------------------------------------------------------------------------
-- | When running auto, in order to prune the auto search tree, we try
-- a homomorphic destruct whenever possible. If that produces any results, we
-- can probably just prune the other side.
destructOrHomoAuto :: HyInfo CType -> TacticsM ()
destructOrHomoAuto hi = tracing "destructOrHomoAuto" $ do
  jdg <- goal
  let g  = unCType $ jGoal jdg
      ty = unCType $ hi_type hi

  attemptWhen
      (rule $ destruct' False (\dc jdg ->
        buildDataCon False jdg dc $ snd $ splitAppTys g) hi)
      (rule $ destruct' False (const newSubgoal) hi)
    $ case (splitTyConApp_maybe g, splitTyConApp_maybe ty) of
        (Just (gtc, _), Just (tytc, _)) -> gtc == tytc
        _ -> False


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches.
destruct :: HyInfo CType -> TacticsM ()
destruct hi = requireConcreteHole $ tracing "destruct(user)" $
  rule $ destruct' False (const newSubgoal) hi


------------------------------------------------------------------------------
-- | Case split, and leave holes in the matches. Performs record punning.
destructPun :: HyInfo CType -> TacticsM ()
destructPun hi = requireConcreteHole $ tracing "destructPun(user)" $
  rule $ destruct' True (const newSubgoal) hi


------------------------------------------------------------------------------
-- | Case split, using the same data constructor in the matches.
homo :: HyInfo CType -> TacticsM ()
homo hi = requireConcreteHole . tracing "homo" $ do
  jdg <- goal
  let g = jGoal jdg

  -- Ensure that every data constructor in the domain type is covered in the
  -- codomain; otherwise 'homo' will produce an ill-typed program.
  case (uncoveredDataCons (coerce $ hi_type hi) (coerce g)) of
    Just uncovered_dcs ->
      unless (S.null uncovered_dcs) $
        throwError  $ TacticPanic "Can't cover every datacon in domain"
    _ -> throwError $ TacticPanic "Unable to fetch datacons"

  rule
    $ destruct'
        False
        (\dc jdg -> buildDataCon False jdg dc $ snd $ splitAppTys $ unCType $ jGoal jdg)
    $ hi


------------------------------------------------------------------------------
-- | LambdaCase split, and leave holes in the matches.
destructLambdaCase :: TacticsM ()
destructLambdaCase =
  tracing "destructLambdaCase" $ rule $ destructLambdaCase' False (const newSubgoal)


------------------------------------------------------------------------------
-- | LambdaCase split, using the same data constructor in the matches.
homoLambdaCase :: TacticsM ()
homoLambdaCase =
  tracing "homoLambdaCase" $
    rule $ destructLambdaCase' False $ \dc jdg ->
      buildDataCon False jdg dc
        . snd
        . splitAppTys
        . unCType
        $ jGoal jdg


data Saturation = Unsaturated Int
  deriving (Eq, Ord, Show)

pattern Saturated :: Saturation
pattern Saturated = Unsaturated 0


apply :: Saturation -> HyInfo CType -> TacticsM ()
apply (Unsaturated n) hi = tracing ("apply' " <> show (hi_name hi)) $ do
  jdg <- goal
  let g  = jGoal jdg
      ty = unCType $ hi_type hi
      func = hi_name hi
  ty' <- freshTyvars ty
  let (_, _, all_args, ret) = tacticsSplitFunTy ty'
      saturated_args = dropEnd n all_args
      unsaturated_args = takeEnd n all_args
  rule $ \jdg -> do
    unify g (CType $ mkFunTys' unsaturated_args ret)
    ext
        <- fmap unzipTrace
        $ traverse ( newSubgoal
                    . blacklistingDestruct
                    . flip withNewGoal jdg
                    . CType
                    ) saturated_args
    pure $
      ext
        & #syn_used_vals %~ S.insert func
        & #syn_val       %~ mkApply func . fmap unLoc

application :: TacticsM ()
application = overFunctions $ apply Saturated


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors.
split :: TacticsM ()
split = tracing "split(user)" $ do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (dcs, _) -> choice $ fmap splitDataCon dcs


------------------------------------------------------------------------------
-- | Choose between each of the goal's data constructors. Different than
-- 'split' because it won't split a data con if it doesn't result in any new
-- goals.
splitAuto :: TacticsM ()
splitAuto = requireConcreteHole $ tracing "split(auto)" $ do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Nothing -> throwError $ GoalMismatch "split" g
    Just (dcs, _) -> do
      case isSplitWhitelisted jdg of
        True -> choice $ fmap splitDataCon dcs
        False -> do
          choice $ flip fmap dcs $ \dc -> requireNewHoles $
            splitDataCon dc


------------------------------------------------------------------------------
-- | Like 'split', but only works if there is a single matching data
-- constructor for the goal.
splitSingle :: TacticsM ()
splitSingle = tracing "splitSingle" $ do
  jdg <- goal
  let g = jGoal jdg
  case tacticsGetDataCons $ unCType g of
    Just ([dc], _) -> do
      splitDataCon dc
    _ -> throwError $ GoalMismatch "splitSingle" g

------------------------------------------------------------------------------
-- | Like 'split', but prunes any data constructors which have holes.
obvious :: TacticsM ()
obvious = tracing "obvious" $ do
  pruning split $ bool (Just NoProgress) Nothing . null


------------------------------------------------------------------------------
-- | Sorry leaves a hole in its extract
sorry :: TacticsM ()
sorry = exact $ var' $ mkVarOcc "_"


------------------------------------------------------------------------------
-- | Sorry leaves a hole in its extract
metaprogram :: TacticsM ()
metaprogram = exact $ MetaprogramSyntax ""


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
-- | Attempt to instantiate the given ConLike to solve the goal.
--
-- INVARIANT: Assumes the given ConLike is appropriate to construct the type
-- with.
splitConLike :: ConLike -> TacticsM ()
splitConLike dc =
  requireConcreteHole $ tracing ("splitDataCon:" <> show dc) $ rule $ \jdg -> do
    let g = jGoal jdg
    case splitTyConApp_maybe $ unCType g of
      Just (_, apps) -> do
        buildDataCon True (unwhitelistingSplit jdg) dc apps
      Nothing -> throwError $ GoalMismatch "splitDataCon" g

------------------------------------------------------------------------------
-- | Attempt to instantiate the given data constructor to solve the goal.
--
-- INVARIANT: Assumes the given datacon is appropriate to construct the type
-- with.
splitDataCon :: DataCon -> TacticsM ()
splitDataCon = splitConLike . RealDataCon


------------------------------------------------------------------------------
-- | Perform a case split on each top-level argument. Used to implement the
-- "Destruct all function arguments" action.
destructAll :: TacticsM ()
destructAll = do
  jdg <- goal
  let args = fmap fst
           $ sortOn snd
           $ mapMaybe (\(hi, prov) ->
              case prov of
                TopLevelArgPrv _ idx _ -> pure (hi, idx)
                _ -> Nothing
                )
           $ fmap (\hi -> (hi, hi_provenance hi))
           $ filter (isAlgType . unCType . hi_type)
           $ unHypothesis
           $ jHypothesis jdg
  for_ args $ \arg -> do
    subst <- getSubstForJudgement =<< goal
    destruct $ fmap (coerce substTy subst) arg

--------------------------------------------------------------------------------
-- | User-facing tactic to implement "Use constructor <x>"
userSplit :: OccName -> TacticsM ()
userSplit occ = do
  jdg <- goal
  let g = jGoal jdg
  -- TODO(sandy): It's smelly that we need to find the datacon to generate the
  -- code action, send it as a string, and then look it up again. Can we push
  -- this over LSP somehow instead?
  case splitTyConApp_maybe $ unCType g of
    Just (tc, _) -> do
      case find (sloppyEqOccName occ . occName . dataConName)
             $ tyConDataCons tc of
        Just dc -> splitDataCon dc
        Nothing -> throwError $ NotInScope occ
    Nothing -> throwError $ NotInScope occ


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


refine :: TacticsM ()
refine = intros <%> splitSingle


auto' :: Int -> TacticsM ()
auto' 0 = throwError NoProgress
auto' n = do
  let loop = auto' (n - 1)
  try intros
  choice
    [ overFunctions $ \fname -> do
        requireConcreteHole $ apply Saturated fname
        loop
    , overAlgebraicTerms $ \aname -> do
        destructAuto aname
        loop
    , splitAuto >> loop
    , assumption >> loop
    , recursion
    ]

overFunctions :: (HyInfo CType -> TacticsM ()) -> TacticsM ()
overFunctions =
  attemptOn $ filter (isFunction . unCType . hi_type)
           . unHypothesis
           . jHypothesis

overAlgebraicTerms :: (HyInfo CType -> TacticsM ()) -> TacticsM ()
overAlgebraicTerms =
  attemptOn jAcceptableDestructTargets


allNames :: Judgement -> Set OccName
allNames = hyNamesInScope . jHypothesis


applyMethod :: Class -> PredType -> OccName -> TacticsM ()
applyMethod cls df method_name = do
  case find ((== method_name) . occName) $ classMethods cls of
    Just method -> do
      let (_, apps) = splitAppTys df
      let ty = piResultTys (idType method) apps
      apply Saturated $ HyInfo method_name (ClassMethodPrv $ Uniquely cls) $ CType ty
    Nothing -> throwError $ NotInScope method_name


applyByName :: OccName -> TacticsM ()
applyByName name = do
  g <- goal
  choice $ (unHypothesis (jHypothesis g)) <&> \hi ->
    case hi_name hi == name of
      True  -> apply Saturated hi
      False -> empty


------------------------------------------------------------------------------
-- | Make a function application where the function being applied itself is
-- a hole.
applyByType :: Type -> TacticsM ()
applyByType ty = tracing ("applyByType " <> show ty) $ do
  jdg <- goal
  let g  = jGoal jdg
  ty' <- freshTyvars ty
  let (_, _, args, ret) = tacticsSplitFunTy ty'
  rule $ \jdg -> do
    unify g (CType ret)
    ext
        <- fmap unzipTrace
        $ traverse ( newSubgoal
                    . blacklistingDestruct
                    . flip withNewGoal jdg
                    . CType
                    ) args
    app <- newSubgoal . blacklistingDestruct $ withNewGoal (CType ty) jdg
    pure $
      fmap noLoc $
        foldl' (@@)
          <$> fmap unLoc app
          <*> fmap (fmap unLoc) ext


------------------------------------------------------------------------------
-- | Make an n-ary function call of the form
-- @(_ :: forall a b. a -> a -> b) _ _@.
nary :: Int -> TacticsM ()
nary n =
  applyByType $
    mkInvForAllTys [alphaTyVar, betaTyVar] $
      mkFunTys' (replicate n alphaTy) betaTy


self :: TacticsM ()
self =
  fmap listToMaybe getCurrentDefinitions >>= \case
    Just (self, _) -> useNameFromContext (apply Saturated) self
    Nothing -> throwError $ TacticPanic "no defining function"


------------------------------------------------------------------------------
-- | Perform a catamorphism when destructing the given 'HyInfo'. This will
-- result in let binding, making values that call the defining function on each
-- destructed value.
cata :: HyInfo CType -> TacticsM ()
cata hi = do
  (_, _, calling_args, _)
      <- tacticsSplitFunTy . unCType <$> getDefiningType
  freshened_args <- traverse freshTyvars calling_args
  diff <- hyDiff $ destruct hi

  -- For for every destructed term, check to see if it can unify with any of
  -- the arguments to the calling function. If it doesn't, we don't try to
  -- perform a cata on it.
  unifiable_diff <- flip filterM (unHypothesis diff) $ \hi ->
    flip anyM freshened_args $ \ty ->
      canUnify (hi_type hi) $ CType ty

  rule $
    letForEach
      (mkVarOcc . flip mappend "_c" . occNameString)
      (\hi -> self >> commit (assume $ hi_name hi) assumption)
      $ Hypothesis unifiable_diff


------------------------------------------------------------------------------
-- | Deeply nest an unsaturated function onto itself
nested :: OccName -> TacticsM ()
nested = deepening . use (Unsaturated 1)


------------------------------------------------------------------------------
-- | Repeatedly bind a tactic on its first hole
deep :: Int -> TacticsM () -> TacticsM ()
deep 0 _ = pure ()
deep n t = foldr1 bindOne $ replicate n t


------------------------------------------------------------------------------
-- | Try 'deep' for arbitrary depths.
deepening :: TacticsM () -> TacticsM ()
deepening t =
  asum $ fmap (flip deep t) [0 .. 100]


bindOne :: TacticsM a -> TacticsM a -> TacticsM a
bindOne t t1 = t <@> [t1]


collapse :: TacticsM ()
collapse = do
  g <- goal
  let terms = unHypothesis $ hyFilter ((jGoal g ==) . hi_type) $ jLocalHypothesis g
  case terms of
    [hi] -> assume $ hi_name hi
    _    -> nary (length terms) <@> fmap (assume . hi_name) terms


with_arg :: TacticsM ()
with_arg = rule $ \jdg -> do
  let g = jGoal jdg
  fresh_ty <- freshTyvars $ mkInvForAllTys [alphaTyVar] alphaTy
  a <- newSubgoal $ withNewGoal (CType fresh_ty) jdg
  f <- newSubgoal $ withNewGoal (coerce mkFunTys' [fresh_ty] g) jdg
  pure $ fmap noLoc $ (@@) <$> fmap unLoc f <*> fmap unLoc a


------------------------------------------------------------------------------
-- | Determine the difference in hypothesis due to running a tactic. Also, it
-- runs the tactic.
hyDiff :: TacticsM () -> TacticsM (Hypothesis CType)
hyDiff m = do
  g <- unHypothesis . jEntireHypothesis <$> goal
  let g_len = length g
  m
  g' <- unHypothesis . jEntireHypothesis <$> goal
  pure $ Hypothesis $ take (length g' - g_len) g'

