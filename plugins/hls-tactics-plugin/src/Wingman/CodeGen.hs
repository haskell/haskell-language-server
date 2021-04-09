{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module Wingman.CodeGen
  ( module Wingman.CodeGen
  , module Wingman.CodeGen.Utils
  ) where


import           ConLike
import           Control.Lens ((%~), (<>~), (&))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bool (bool)
import           Data.Generics.Labels ()
import           Data.List
import           Data.Monoid (Endo(..))
import qualified Data.Set as S
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           GhcPlugins (isSymOcc)
import           PatSyn
import           Type hiding (Var)
import           Wingman.CodeGen.Utils
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Judgements.Theta
import           Wingman.Machinery
import           Wingman.Naming
import           Wingman.Types


destructMatches
    :: (ConLike -> Judgement -> Rule)
       -- ^ How to construct each match
    -> Maybe OccName
       -- ^ Scrutinee
    -> CType
       -- ^ Type being destructed
    -> Judgement
    -> RuleM (Synthesized [RawMatch])
destructMatches f scrut t jdg = do
  let hy = jEntireHypothesis jdg
      g  = jGoal jdg
  case splitTyConApp_maybe $ unCType t of
    Nothing -> throwError $ GoalMismatch "destruct" g
    Just (tc, apps) -> do
      let dcs = tyConDataCons tc
      case dcs of
        [] -> throwError $ GoalMismatch "destruct" g
        _ -> fmap unzipTrace $ for dcs $ \dc -> do
          let con = RealDataCon dc
              ev = concatMap mkEvidence $ dataConInstArgTys dc apps
              -- We explicitly do not need to add the method hypothesis to
              -- #syn_scoped
              method_hy = foldMap evidenceToHypothesis ev
              args = conLikeInstOrigArgTys' con apps
          modify $ appEndo $ foldMap (Endo . evidenceToSubst) ev
          subst <- gets ts_unifier
          names <- mkManyGoodNames (hyNamesInScope hy) args
          let hy' = patternHypothesis scrut con jdg
                  $ zip names
                  $ coerce args
              j = fmap (CType . substTyAddInScope subst . unCType)
                $ introduce hy'
                $ introduce method_hy
                $ withNewGoal g jdg
          ext <- f con j
          pure $ ext
            & #syn_trace %~ rose ("match " <> show dc <> " {" <> intercalate ", " (fmap show names) <> "}")
                          . pure
            & #syn_scoped <>~ hy'
            & #syn_val     %~ match [mkDestructPat con names] . unLoc


------------------------------------------------------------------------------
-- | Produces a pattern for a data con and the names of its fields.
mkDestructPat :: ConLike -> [OccName] -> Pat GhcPs
mkDestructPat con names
  | RealDataCon dcon <- con
  , isTupleDataCon dcon =
      tuple pat_args
  | otherwise =
      infixifyPatIfNecessary con $
        conP
          (coerceName $ conLikeName con)
          pat_args
  where
    pat_args = fmap bvar' names


infixifyPatIfNecessary :: ConLike -> Pat GhcPs -> Pat GhcPs
infixifyPatIfNecessary dcon x
  | conLikeIsInfix dcon =
      case x of
        ConPatIn op (PrefixCon [lhs, rhs]) ->
          ConPatIn op $ InfixCon lhs rhs
        y -> y
  | otherwise = x



unzipTrace :: [Synthesized a] -> Synthesized [a]
unzipTrace = sequenceA


-- | Essentially same as 'dataConInstOrigArgTys' in GHC,
--  but only accepts universally quantified types as the second arguments
--  and automatically introduces existentials.
--
-- NOTE: The behaviour depends on GHC's 'dataConInstOrigArgTys'.
--       We need some tweaks if the compiler changes the implementation.
conLikeInstOrigArgTys'
  :: ConLike
      -- ^ 'DataCon'structor
  -> [Type]
      -- ^ /Universally/ quantified type arguments to a result type.
      --   It /MUST NOT/ contain any dictionaries, coercion and existentials.
      --
      --   For example, for @MkMyGADT :: b -> MyGADT a c@, we
      --   must pass @[a, c]@ as this argument but not @b@, as @b@ is an existential.
  -> [Type]
      -- ^ Types of arguments to the ConLike with returned type is instantiated with the second argument.
conLikeInstOrigArgTys' con uniTys =
  let exvars = conLikeExTys con
   in conLikeInstOrigArgTys con $
        uniTys ++ fmap mkTyVarTy exvars
      -- Rationale: At least in GHC <= 8.10, 'dataConInstOrigArgTys'
      -- unifies the second argument with DataCon's universals followed by existentials.
      -- If the definition of 'dataConInstOrigArgTys' changes,
      -- this place must be changed accordingly.


conLikeExTys :: ConLike -> [TyCoVar]
conLikeExTys (RealDataCon d) = dataConExTys d
conLikeExTys (PatSynCon p) = patSynExTys p

patSynExTys :: PatSyn -> [TyCoVar]
patSynExTys ps = patSynExTyVars ps


------------------------------------------------------------------------------
-- | Combinator for performing case splitting, and running sub-rules on the
-- resulting matches.

destruct' :: (ConLike -> Judgement -> Rule) -> HyInfo CType -> Judgement -> Rule
destruct' f hi jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let term = hi_name hi
  ext
      <- destructMatches
           f
           (Just term)
           (hi_type hi)
           $ disallowing AlreadyDestructed [term] jdg
  pure $ ext
    & #syn_trace     %~ rose ("destruct " <> show term) . pure
    & #syn_used_vals %~ S.insert term
    & #syn_val       %~ noLoc . case' (var' term)


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destructLambdaCase' :: (ConLike -> Judgement -> Rule) -> Judgement -> Rule
destructLambdaCase' f jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let g  = jGoal jdg
  case splitFunTy_maybe (unCType g) of
    Just (arg, _) | isAlgType arg ->
      fmap (fmap noLoc lambdaCase) <$>
        destructMatches f Nothing (CType arg) jdg
    _ -> throwError $ GoalMismatch "destructLambdaCase'" g


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Bool       -- Should we blacklist destruct?
    -> Judgement
    -> ConLike            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (Synthesized (LHsExpr GhcPs))
buildDataCon should_blacklist jdg dc tyapps = do
  let args = conLikeInstOrigArgTys' dc tyapps
  ext
      <- fmap unzipTrace
       $ traverse ( \(arg, n) ->
                    newSubgoal
                  . filterSameTypeFromOtherPositions dc n
                  . bool id blacklistingDestruct should_blacklist
                  . flip withNewGoal jdg
                  $ CType arg
                  ) $ zip args [0..]
  pure $ ext
    & #syn_trace %~ rose (show dc) . pure
    & #syn_val   %~ mkCon dc tyapps


------------------------------------------------------------------------------
-- | Make a function application, correctly handling the infix case.
mkApply :: OccName -> [HsExpr GhcPs] -> LHsExpr GhcPs
mkApply occ (lhs : rhs : more)
  | isSymOcc occ
  = noLoc $ foldl' (@@) (op lhs (coerceName occ) rhs) more
mkApply occ args = noLoc $ foldl' (@@) (var' occ) args

