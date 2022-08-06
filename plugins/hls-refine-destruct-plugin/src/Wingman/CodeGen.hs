{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Wingman.CodeGen
  ( module Wingman.CodeGen
  , module Wingman.CodeGen.Utils
  ) where


import           Control.Lens ((%~), (&))
import           Data.Bifunctor (second)
import           Data.Bool (bool)
import           Data.Functor ((<&>))
import           Data.Generics.Labels ()
import           Data.List
import qualified Data.Set as S
import           Data.Traversable
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen (occNameToStr)
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           Wingman.CodeGen.Utils
import           Wingman.GHC
import           Wingman.Judgements
import           Wingman.Machinery
import           Wingman.Naming
import           Wingman.Types
import GHC (EpAnn(..), emptyComments)


destructMatches
    :: Bool
    -> (ConLike -> Judgement -> Rule)
       -- ^ How to construct each match
    -> Maybe OccName
       -- ^ Scrutinee
    -> CType
       -- ^ Type being destructed
    -> Judgement
    -> RuleM (Synthesized [RawMatch])
-- TODO(sandy): In an ideal world, this would be the same codepath as
-- 'destructionFor'. Make sure to change that if you ever change this.
destructMatches use_field_puns f scrut t jdg = do
  let hy = jEntireHypothesis jdg
  case tacticsGetDataCons $ unCType t of
    Nothing -> cut -- throwError $ GoalMismatch "destruct" g
    Just (dcs, apps) ->
      fmap unzipTrace $ for dcs $ \dc -> do
        let con = RealDataCon dc
            -- We explicitly do not need to add the method hypothesis to
            -- #syn_scoped
            args = conLikeInstOrigArgTys' con apps

        let names_in_scope = hyNamesInScope hy
            names = mkManyGoodNames (hyNamesInScope hy) args
            (names', destructed) =
              mkDestructPat (bool Nothing (Just names_in_scope) use_field_puns) con names

        let hy' = patternHypothesis scrut con jdg
                $ zip names'
                $ coerce args
        ext <- f con $ introduce hy' jdg
        pure $ ext
          & #syn_val %~ match [destructed] . unLoc


------------------------------------------------------------------------------
-- | Generate just the 'Match'es for a case split on a specific type.
destructionFor :: Hypothesis a -> Type -> Maybe [LMatch GhcPs (LHsExpr GhcPs)]
-- TODO(sandy): In an ideal world, this would be the same codepath as
-- 'destructMatches'. Make sure to change that if you ever change this.
destructionFor hy t = do
  case tacticsGetDataCons t of
    Nothing -> Nothing
    Just ([], _) -> Nothing
    Just (dcs, apps) -> do
      for dcs $ \dc -> do
        let con   = RealDataCon dc
            args  = conLikeInstOrigArgTys' con apps
            names = mkManyGoodNames (hyNamesInScope hy) args
        pure
          . noLocA
          . Match
              EpAnnNotUsed
              CaseAlt
              [toPatCompat $ snd $ mkDestructPat Nothing con names]
          . GRHSs emptyComments (pure $ noLoc $ GRHS EpAnnNotUsed [] $ noLocA $ var "_")
          $ EmptyLocalBinds NoExtField



------------------------------------------------------------------------------
-- | Produces a pattern for a data con and the names of its fields.
mkDestructPat :: Maybe (S.Set OccName) -> ConLike -> [OccName] -> ([OccName], Pat GhcPs)
mkDestructPat already_in_scope con names
  | RealDataCon dcon <- con
  , isTupleDataCon dcon =
      (names, tuple pat_args)
  | fields@(_:_) <- zip (conLikeFieldLabels con) names
  , Just in_scope <- already_in_scope =
      let (names', rec_fields) =
            unzip $ fields <&> \(label, name) -> do
              let label_occ = mkVarOccFS $ flLabel label
              case S.member label_occ in_scope of
                -- We have a shadow, so use the generated name instead
                True ->
                  (name,) $ noLocA $
                    HsRecField
                      EpAnnNotUsed
                      (noLoc $ mkFieldOcc $ noLocA $ Unqual label_occ)
                      (noLocA $ bvar' name)
                      False
                -- No shadow, safe to use a pun
                False ->
                  (label_occ,) $ noLocA $
                    HsRecField
                      EpAnnNotUsed
                      (noLoc $ mkFieldOcc $ noLocA $ Unqual label_occ)
                      (noLocA $ bvar' label_occ)
                      True

        in (names', )
         $ ConPatIn (noLoc $ Unqual $ occName $ conLikeName con)
         $ RecCon
         $ HsRecFields rec_fields Nothing
  | otherwise =
      (names, ) $ infixifyPatIfNecessary con $
        conP
          (coerceName $ conLikeName con)
          pat_args
  where
    pat_args = fmap bvar' names


infixifyPatIfNecessary :: ConLike -> Pat GhcPs -> Pat GhcPs
infixifyPatIfNecessary dcon x
  | conLikeIsInfix dcon =
      case x of
        ConPatIn op (PrefixCon _ [lhs, rhs]) ->
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
   in fmap scaledThing $ conLikeInstOrigArgTys con $
        uniTys ++ fmap mkTyVarTy exvars
      -- Rationale: At least in GHC <= 8.10, 'dataConInstOrigArgTys'
      -- unifies the second argument with DataCon's universals followed by existentials.
      -- If the definition of 'dataConInstOrigArgTys' changes,
      -- this place must be changed accordingly.


conLikeExTys :: ConLike -> [TyCoVar]
conLikeExTys (RealDataCon d) = dataConExTyCoVars d
conLikeExTys (PatSynCon p) = patSynExTys p

patSynExTys :: PatSyn -> [TyCoVar]
patSynExTys ps = patSynExTyVars ps


------------------------------------------------------------------------------
-- | Combinator for performing case splitting, and running sub-rules on the
-- resulting matches.

destruct' :: Bool -> (ConLike -> Judgement -> Rule) -> HyInfo CType -> Judgement -> Rule
destruct' use_field_puns f hi jdg = do
  let term = hi_name hi
  ext
      <- destructMatches
           use_field_puns
           f
           (Just term)
           (hi_type hi)
           $ disallowing AlreadyDestructed (S.singleton term) jdg
  pure $ ext
    & #syn_val       %~ noLocA . case' (var' term)


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destructLambdaCase' :: Bool -> (ConLike -> Judgement -> Rule) -> Judgement -> Rule
destructLambdaCase' use_field_puns f jdg = do
  let g  = jGoal jdg
  case splitFunTy_maybe (unCType g) of
#if __GLASGOW_HASKELL__ >= 900
    Just (_multiplicity, arg, _) | isAlgType arg ->
#else
    Just (arg, _) | isAlgType arg ->
#endif
      fmap (fmap noLocA lambdaCase) <$>
        destructMatches use_field_puns f Nothing (CType arg) jdg
    _ -> cut -- throwError $ GoalMismatch "destructLambdaCase'" g


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Judgement
    -> ConLike            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (Synthesized (LHsExpr GhcPs))
buildDataCon jdg dc tyapps = do
  args <- case dc of
    RealDataCon dc' -> do
      let (_ , _, args) = dataConInstSig dc' tyapps
      pure args
    _ ->
      -- If we have a 'PatSyn', we can't continue, since there is no
      -- 'dataConInstSig' equivalent for 'PatSyn's. I don't think this is
      -- a fundamental problem, but I don't know enough about the GHC internals
      -- to implement it myself.
      --
      -- Fortunately, this isn't an issue in practice, since 'PatSyn's are
      -- never in the hypothesis.
      cut -- throwError $ TacticPanic "Can't build Pattern constructors yet"
  ext
      <- fmap unzipTrace
       $ traverse ( \(arg, n) ->
                    newSubgoal
                  . filterSameTypeFromOtherPositions dc n
                  . flip withNewGoal jdg
                  $ CType arg
                  ) $ zip args [0..]
  pure $ ext
    & #syn_val   %~ mkCon dc tyapps


------------------------------------------------------------------------------
-- | Make a function application, correctly handling the infix case.
mkApply :: OccName -> [HsExpr GhcPs] -> LHsExpr GhcPs
mkApply occ (lhs : rhs : more)
  | isSymOcc occ
  = noLocA $ foldl' (@@) (op lhs (coerceName occ) rhs) more
mkApply occ args = noLocA $ foldl' (@@) (var' occ) args


------------------------------------------------------------------------------
-- | Run a tactic over each term in the given 'Hypothesis', binding the results
-- of each in a let expression.
letForEach
    :: (OccName -> OccName)           -- ^ How to name bound variables
    -> (HyInfo CType -> TacticsM ())  -- ^ The tactic to run
    -> Hypothesis CType               -- ^ Terms to generate bindings for
    -> Judgement                      -- ^ The goal of original hole
    -> RuleM (Synthesized (LHsExpr GhcPs))
letForEach rename solve (unHypothesis -> hy) jdg = do
  case hy of
    [] -> newSubgoal jdg
    _ -> do
      let g = jGoal jdg
      terms <- fmap sequenceA $ for hy $ \hi -> do
        let name = rename $ hi_name hi
        let generalized_let_ty = CType alphaTy
        res <- tacticToRule (withNewGoal generalized_let_ty jdg) $ solve hi
        pure $ fmap ((name,) . unLoc) res
      let hy' = fmap (g <$) $ syn_val terms
          matches = fmap (fmap (\(occ, expr) -> valBind (occNameToStr occ) expr)) terms
      g <- fmap (fmap unLoc) $ newSubgoal $ introduce (userHypothesis hy') jdg
      pure $ fmap noLocA $ let' <$> matches <*> g


------------------------------------------------------------------------------
-- | Let-bind the given occname judgement pairs.
nonrecLet
    :: [(OccName, Judgement)]
    -> Judgement
    -> RuleM (Synthesized (LHsExpr GhcPs))
nonrecLet occjdgs jdg = do
  occexts <- traverse newSubgoal $ fmap snd occjdgs
  ext     <- newSubgoal
           $ introduce (userHypothesis $ fmap (second jGoal) occjdgs) jdg
  pure $ fmap noLocA $
    let'
      <$> traverse
            (\(occ, ext) -> valBind (occNameToStr occ) <$> fmap unLoc ext)
            (zip (fmap fst occjdgs) occexts)
      <*> fmap unLoc ext


------------------------------------------------------------------------------
-- | Converts a function application into applicative form
idiomize :: LHsExpr GhcPs -> LHsExpr GhcPs
idiomize x = noLocA $ case unLoc x of
  HsApp _ (L _ (HsVar _ (L _ x))) gshgp3 ->
    op (bvar' $ occName x) "<$>" (unLoc gshgp3)
  HsApp _ gsigp gshgp3 ->
    op (unLoc $ idiomize gsigp) "<*>" (unLoc gshgp3)
  RecordCon _ con (HsRecFields flds _) ->
    unLoc $ idiomize $ noLocA $ foldl' (@@) (HsVar noExtField con) $ fmap (unLoc . hsRecFieldArg . unLoc) flds
  y -> y

