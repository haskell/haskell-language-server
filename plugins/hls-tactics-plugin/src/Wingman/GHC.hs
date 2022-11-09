{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Wingman.GHC where

import           Control.Monad.State
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Bool (bool)
import           Data.Coerce (coerce)
import           Data.Function (on)
import           Data.Functor ((<&>))
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Traversable
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           GHC.SourceGen (lambda)
import           Generics.SYB (Data, everything, everywhere, listify, mkQ, mkT)
import           Wingman.StaticPlugin (pattern MetaprogramSyntax)
import           Wingman.Types

#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Utils.TcType
#endif


tcTyVar_maybe :: Type -> Maybe Var
tcTyVar_maybe ty | Just ty' <- tcView ty = tcTyVar_maybe ty'
tcTyVar_maybe (CastTy ty _) = tcTyVar_maybe ty  -- look through casts, as
                                                -- this is only used for
                                                -- e.g., FlexibleContexts
tcTyVar_maybe (TyVarTy v)   = Just v
tcTyVar_maybe _             = Nothing


instantiateType :: Type -> ([TyVar], Type)
instantiateType t = do
  let vs  = tyCoVarsOfTypeList t
      vs' = fmap cloneTyVar vs
      subst = foldr (\(v,t) a -> extendTCvSubst a v $ TyVarTy t) emptyTCvSubst
            $ zip vs vs'
   in (vs', substTy subst t)


cloneTyVar :: TyVar -> TyVar
cloneTyVar t =
  let uniq = getUnique t
      some_magic_char = 'w' -- 'w' for wingman ;D
   in setVarUnique t $ newTagUnique uniq some_magic_char


------------------------------------------------------------------------------
-- | Is this a function type?
isFunction :: Type -> Bool
isFunction (tacticsSplitFunTy -> (_, _, [], _)) = False
isFunction _                                    = True


------------------------------------------------------------------------------
-- | Split a function, also splitting out its quantified variables and theta
-- context.
tacticsSplitFunTy :: Type -> ([TyVar], ThetaType, [Scaled Type], Type)
tacticsSplitFunTy t
  = let (vars, theta, t') = tcSplitNestedSigmaTys t
        (args, res) = tcSplitFunTys t'
     in (vars, theta, args, res)


------------------------------------------------------------------------------
-- | Rip the theta context out of a regular type.
tacticsThetaTy :: Type -> ThetaType
tacticsThetaTy (tcSplitSigmaTy -> (_, theta,  _)) = theta


------------------------------------------------------------------------------
-- | Get the data cons of a type, if it has any.
tacticsGetDataCons :: Type -> Maybe ([DataCon], [Type])
tacticsGetDataCons ty
  | Just (_, ty') <- tcSplitForAllTyVarBinder_maybe ty
  = tacticsGetDataCons ty'
tacticsGetDataCons ty
  | Just _ <- algebraicTyCon ty
  = splitTyConApp_maybe ty <&> \(tc, apps) ->
      ( filter (not . dataConCannotMatch apps) $ tyConDataCons tc
      , apps
      )
tacticsGetDataCons _ = Nothing

------------------------------------------------------------------------------
-- | Instantiate all of the quantified type variables in a type with fresh
-- skolems.
freshTyvars :: MonadState TacticState m => Type -> m Type
freshTyvars t = do
  let (tvs, _, _, _) = tacticsSplitFunTy t
  reps <- fmap M.fromList
        $ for tvs $ \tv -> do
            uniq <- freshUnique
            pure (tv, setTyVarUnique tv uniq)
  pure $
    everywhere
      (mkT $ \tv -> M.findWithDefault tv tv reps
      ) $ snd $ tcSplitForAllTyVars t


------------------------------------------------------------------------------
-- | Given a datacon, extract its record fields' names and types. Returns
-- nothing if the datacon is not a record.
getRecordFields :: ConLike -> Maybe [(OccName, CType)]
getRecordFields dc =
  case conLikeFieldLabels dc of
    [] -> Nothing
    lbls -> for lbls $ \lbl -> do
      let ty = conLikeFieldType dc $ flLabel lbl
      pure (mkVarOccFS $ flLabel lbl, CType ty)


------------------------------------------------------------------------------
-- | Is this an algebraic type?
algebraicTyCon :: Type -> Maybe TyCon
algebraicTyCon ty
  | Just (_, ty') <- tcSplitForAllTyVarBinder_maybe ty
  = algebraicTyCon ty'
algebraicTyCon (splitTyConApp_maybe -> Just (tycon, _))
  | tycon == intTyCon    = Nothing
  | tycon == floatTyCon  = Nothing
  | tycon == doubleTyCon = Nothing
  | tycon == charTyCon   = Nothing
  | tycon == funTyCon    = Nothing
  | otherwise = Just tycon
algebraicTyCon _ = Nothing


------------------------------------------------------------------------------
-- | We can't compare 'RdrName' for equality directly. Instead, sloppily
-- compare them by their 'OccName's.
eqRdrName :: RdrName -> RdrName -> Bool
eqRdrName = (==) `on` occNameString . occName


------------------------------------------------------------------------------
-- | Compare two 'OccName's for unqualified equality.
sloppyEqOccName :: OccName -> OccName -> Bool
sloppyEqOccName = (==) `on` occNameString


------------------------------------------------------------------------------
-- | Does this thing contain any references to 'HsVar's with the given
-- 'RdrName'?
containsHsVar :: Data a => RdrName -> a -> Bool
containsHsVar name x = not $ null $ listify (
  \case
    ((HsVar _ (L _ a)) :: HsExpr GhcPs) | eqRdrName a name -> True
    _                                                      -> False
  ) x


------------------------------------------------------------------------------
-- | Does this thing contain any holes?
containsHole :: Data a => a -> Bool
containsHole x = not $ null $ listify (
  \case
    ((HsVar _ (L _ name)) :: HsExpr GhcPs) -> isHole $ occName name
    MetaprogramSyntax _                    -> True
    _                                      -> False
  ) x


------------------------------------------------------------------------------
-- | Check if an 'OccName' is a hole
isHole :: OccName -> Bool
-- TODO(sandy): Make this more robust
isHole = isPrefixOf "_" . occNameString


------------------------------------------------------------------------------
-- | Get all of the referenced occnames.
allOccNames :: Data a => a -> Set OccName
allOccNames = everything (<>) $ mkQ mempty $ \case
    a -> S.singleton a


------------------------------------------------------------------------------
-- | Unpack the relevant parts of a 'Match'
#if __GLASGOW_HASKELL__ >= 900
pattern AMatch :: HsMatchContext (NoGhcTc GhcPs) -> [Pat GhcPs] -> HsExpr GhcPs -> Match GhcPs (LHsExpr GhcPs)
#else
pattern AMatch :: HsMatchContext (NameOrRdrName (IdP GhcPs)) -> [Pat GhcPs] -> HsExpr GhcPs -> Match GhcPs (LHsExpr GhcPs)
#endif
pattern AMatch ctx pats body <-
  Match { m_ctxt = ctx
        , m_pats = fmap fromPatCompat -> pats
        , m_grhss = UnguardedRHSs (unLoc -> body)
        }


pattern SingleLet :: IdP GhcPs -> [Pat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
pattern SingleLet bind pats val expr <-
  HsLet _
    (HsValBinds _
      (ValBinds _ (bagToList ->
        [L _ (FunBind {fun_id = (L _ bind), fun_matches = (MG _ (L _ [L _ (AMatch _ pats val)]) _)})]) _))
    (L _ expr)


------------------------------------------------------------------------------
-- | A pattern over the otherwise (extremely) messy AST for lambdas.
pattern Lambda :: [Pat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs
pattern Lambda pats body <-
  HsLam _
    MG {mg_alts = L _ [L _ (AMatch _ pats body) ]}
  where
    -- If there are no patterns to bind, just stick in the body
    Lambda [] body   = body
    Lambda pats body = lambda pats body


------------------------------------------------------------------------------
-- | A GRHS that contains no guards.
pattern UnguardedRHSs :: LHsExpr p -> GRHSs p (LHsExpr p)
pattern UnguardedRHSs body <-
  GRHSs {grhssGRHSs = [L _ (GRHS _ [] body)]}


------------------------------------------------------------------------------
-- | A match with a single pattern. Case matches are always 'SinglePatMatch'es.
pattern SinglePatMatch :: PatCompattable p => Pat p -> LHsExpr p -> Match p (LHsExpr p)
pattern SinglePatMatch pat body <-
  Match { m_pats = [fromPatCompat -> pat]
        , m_grhss = UnguardedRHSs body
        }


------------------------------------------------------------------------------
-- | Helper function for defining the 'Case' pattern.
unpackMatches :: PatCompattable p => [Match p (LHsExpr p)] -> Maybe [(Pat p, LHsExpr p)]
unpackMatches [] = Just []
unpackMatches (SinglePatMatch pat body : matches) =
  ((pat, body):) <$> unpackMatches matches
unpackMatches _ = Nothing


------------------------------------------------------------------------------
-- | A pattern over the otherwise (extremely) messy AST for lambdas.
pattern Case :: PatCompattable p => HsExpr p -> [(Pat p, LHsExpr p)] -> HsExpr p
pattern Case scrutinee matches <-
  HsCase _ (L _ scrutinee)
    MG {mg_alts = L _ (fmap unLoc -> unpackMatches -> Just matches)}

------------------------------------------------------------------------------
-- | Like 'Case', but for lambda cases.
pattern LamCase :: PatCompattable p => [(Pat p, LHsExpr p)] -> HsExpr p
pattern LamCase matches <-
  HsLamCase _
    MG {mg_alts = L _ (fmap unLoc -> unpackMatches -> Just matches)}


------------------------------------------------------------------------------
-- | Can ths type be lambda-cased?
--
-- Return: 'Nothing' if no
--         @Just False@ if it can't be homomorphic
--         @Just True@ if it can
lambdaCaseable :: Type -> Maybe Bool
#if __GLASGOW_HASKELL__ >= 900
lambdaCaseable (splitFunTy_maybe -> Just (_multiplicity, arg, res))
#else
lambdaCaseable (splitFunTy_maybe -> Just (arg, res))
#endif
  | isJust (algebraicTyCon arg)
  = Just $ isJust $ algebraicTyCon res
lambdaCaseable _ = Nothing

class PatCompattable p where
  fromPatCompat :: PatCompat p -> Pat p
  toPatCompat :: Pat p -> PatCompat p

instance PatCompattable GhcTc where
  fromPatCompat = unLoc
  toPatCompat = noLoc

instance PatCompattable GhcPs where
  fromPatCompat = unLoc
  toPatCompat = noLoc

type PatCompat pass = LPat pass

------------------------------------------------------------------------------
-- | Should make sure it's a fun bind
pattern TopLevelRHS
    :: OccName
    -> [PatCompat GhcTc]
    -> LHsExpr GhcTc
    -> HsLocalBindsLR GhcTc GhcTc
    -> Match GhcTc (LHsExpr GhcTc)
pattern TopLevelRHS name ps body where_binds <-
  Match _
    (FunRhs (L _ (occName -> name)) _ _)
    ps
    (GRHSs _
      [L _ (GRHS _ [] body)] (L _ where_binds))

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe a = MaybeT $ pure a


------------------------------------------------------------------------------
-- | Get the type of an @HsExpr GhcTc@. This is slow and you should prefer to
-- not use it, but sometimes it can't be helped.
typeCheck :: HscEnv -> TcGblEnv -> HsExpr GhcTc -> IO (Maybe Type)
typeCheck hscenv tcg = fmap snd . initDs hscenv tcg . fmap exprType . dsExpr

------------------------------------------------------------------------------
-- | Expand type and data families
normalizeType :: Context -> Type -> Type
normalizeType ctx ty =
  let ty' = expandTyFam ctx ty
   in case tcSplitTyConApp_maybe ty' of
        Just (tc, tys) ->
          -- try to expand any data families
          case tcLookupDataFamInst_maybe (ctxFamInstEnvs ctx) tc tys of
            Just (dtc, dtys, _) -> mkAppTys (mkTyConTy dtc) dtys
            Nothing -> ty'
        Nothing -> ty'

------------------------------------------------------------------------------
-- | Expand type families
expandTyFam :: Context -> Type -> Type
expandTyFam ctx = snd . normaliseType  (ctxFamInstEnvs ctx) Nominal


------------------------------------------------------------------------------
-- | Like 'tcUnifyTy', but takes a list of skolems to prevent unification of.
tryUnifyUnivarsButNotSkolems :: Set TyVar -> CType -> CType -> Maybe TCvSubst
tryUnifyUnivarsButNotSkolems skolems goal inst =
  tryUnifyUnivarsButNotSkolemsMany skolems $ coerce [(goal, inst)]

------------------------------------------------------------------------------
-- | Like 'tryUnifyUnivarsButNotSkolems', but takes a list
-- of pairs of types to unify.
tryUnifyUnivarsButNotSkolemsMany :: Set TyVar -> [(Type, Type)] -> Maybe TCvSubst
tryUnifyUnivarsButNotSkolemsMany skolems (unzip -> (goal, inst)) =
  tcUnifyTys
    (bool BindMe Skolem . flip S.member skolems)
    inst
    goal


updateSubst :: TCvSubst -> TacticState -> TacticState
updateSubst subst s = s { ts_unifier = unionTCvSubst subst (ts_unifier s) }


------------------------------------------------------------------------------
-- | Get the class methods of a 'PredType', correctly dealing with
-- instantiation of quantified class types.
methodHypothesis :: PredType -> Maybe [HyInfo CType]
methodHypothesis ty = do
  (tc, apps) <- splitTyConApp_maybe ty
  cls <- tyConClass_maybe tc
  let methods = classMethods cls
      tvs     = classTyVars cls
      subst   = zipTvSubst tvs apps
  pure $ methods <&> \method ->
    let (_, _, ty) = tcSplitSigmaTy $ idType method
    in ( HyInfo (occName method) (ClassMethodPrv $ Uniquely cls) $ CType $ substTy subst ty
       )

