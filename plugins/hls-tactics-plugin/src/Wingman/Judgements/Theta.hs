{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

module Wingman.Judgements.Theta
  ( Evidence
  , getEvidenceAtHole
  , mkEvidence
  , evidenceToCoercions
  , evidenceToSubst
  , evidenceToHypothesis
  , evidenceToThetaType
  , allEvidenceToSubst
  ) where

import           Control.Applicative (empty)
import           Control.Lens (preview)
import           Data.Coerce (coerce)
import           Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import           Data.Generics.Sum (_Ctor)
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat hiding (empty)
import           Generics.SYB hiding (tyConName, empty, Generic)
import           GHC.Generics
import           Wingman.GHC
import           Wingman.Types

#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Utils.TcType
#endif


------------------------------------------------------------------------------
-- | Something we've learned about the type environment.
data Evidence
    -- | The two types are equal, via a @a ~ b@ relationship
  = EqualityOfTypes Type Type
    -- | We have an instance in scope
  | HasInstance PredType
  deriving (Show, Generic)


------------------------------------------------------------------------------
-- | Given a 'PredType', pull an 'Evidence' out of it.
mkEvidence :: PredType -> [Evidence]
mkEvidence (getEqualityTheta -> Just (a, b))
  = pure $ EqualityOfTypes a b
mkEvidence inst@(tcTyConAppTyCon_maybe -> Just (tyConClass_maybe -> Just cls)) = do
  (_, apps) <- maybeToList $ splitTyConApp_maybe inst
  let tvs     = classTyVars cls
      subst   = zipTvSubst tvs apps
  sc_ev <- traverse (mkEvidence . substTy subst) $ classSCTheta cls
  HasInstance inst : sc_ev
mkEvidence _ = empty


------------------------------------------------------------------------------
-- | Build a set of 'PredType's from the evidence.
evidenceToThetaType :: [Evidence] -> Set CType
evidenceToThetaType evs = S.fromList $ do
  HasInstance t <- evs
  pure $ CType t


------------------------------------------------------------------------------
-- | Compute all the 'Evidence' implicitly bound at the given 'SrcSpan'.
getEvidenceAtHole :: Tracked age SrcSpan -> Tracked age (LHsBinds GhcTc) -> [Evidence]
getEvidenceAtHole (unTrack -> dst)
  = concatMap mkEvidence
  . (everything (<>) $
        mkQ mempty (absBinds dst) `extQ` wrapperBinds dst `extQ` matchBinds dst)
  . unTrack


mkSubst :: Set TyVar -> Type -> Type -> TCvSubst
mkSubst skolems a b =
  let tyvars = S.fromList $ mapMaybe getTyVar_maybe [a, b]
      -- If we can unify our skolems, at least one is no longer a skolem.
      -- Removing them from this set ensures we can get a substitution between
      -- the two. But it's okay to leave them in 'ts_skolems' in general, since
      -- they won't exist after running this substitution.
      skolems' = skolems S.\\ tyvars
   in
  case tryUnifyUnivarsButNotSkolems skolems' (CType a) (CType b) of
    Just subst -> subst
    Nothing -> emptyTCvSubst


substPair :: TCvSubst -> (Type, Type) -> (Type, Type)
substPair subst (ty, ty') = (substTy subst ty, substTy subst ty')


------------------------------------------------------------------------------
-- | Construct a substitution given a list of types that are equal to one
-- another. This is more subtle than it seems, since there might be several
-- equalities for the same type. We must be careful to push the accumulating
-- substitution through each pair of types before adding their equalities.
allEvidenceToSubst :: Set TyVar -> [(Type, Type)] -> TCvSubst
allEvidenceToSubst _ [] = emptyTCvSubst
allEvidenceToSubst skolems ((a, b) : evs) =
  let subst = mkSubst skolems a b
   in unionTCvSubst subst
    $ allEvidenceToSubst skolems
    $ fmap (substPair subst) evs

------------------------------------------------------------------------------
-- | Given some 'Evidence', get a list of which types are now equal.
evidenceToCoercions :: [Evidence] -> [(CType, CType)]
evidenceToCoercions = coerce . mapMaybe (preview $ _Ctor @"EqualityOfTypes")

------------------------------------------------------------------------------
-- | Update our knowledge of which types are equal.
evidenceToSubst :: [Evidence] -> TacticState -> TacticState
evidenceToSubst evs ts =
  updateSubst
    (allEvidenceToSubst (ts_skolems ts) . coerce $ evidenceToCoercions evs)
    ts


------------------------------------------------------------------------------
-- | Get all of the methods that are in scope from this piece of 'Evidence'.
evidenceToHypothesis :: Evidence -> Hypothesis CType
evidenceToHypothesis EqualityOfTypes{} = mempty
evidenceToHypothesis (HasInstance t) =
  Hypothesis . excludeForbiddenMethods . fromMaybe [] $ methodHypothesis t


------------------------------------------------------------------------------
-- | Given @a ~ b@ or @a ~# b@, returns @Just (a, b)@, otherwise @Nothing@.
getEqualityTheta :: PredType -> Maybe (Type, Type)
getEqualityTheta (splitTyConApp_maybe -> Just (tc, [_k, a, b]))
#if __GLASGOW_HASKELL__ > 806
  | tc == eqTyCon
#else
  | nameRdrName (tyConName tc) == eqTyCon_RDR
#endif
  = Just (a, b)
getEqualityTheta (splitTyConApp_maybe -> Just (tc, [_k1, _k2, a, b]))
  | tc == eqPrimTyCon = Just (a, b)
getEqualityTheta _ = Nothing


------------------------------------------------------------------------------
-- | Many operations are defined in typeclasses for performance reasons, rather
-- than being a true part of the class. This function filters out those, in
-- order to keep our hypothesis space small.
excludeForbiddenMethods :: [HyInfo a] -> [HyInfo a]
excludeForbiddenMethods = filter (not . flip S.member forbiddenMethods . hi_name)
  where
    forbiddenMethods :: Set OccName
    forbiddenMethods = S.map mkVarOcc $ S.fromList
      [ -- monadfail
        "fail"
        -- show
      , "showsPrec", "showList"
        -- functor
      , "<$"
        -- applicative
      , "liftA2", "<*", "*>"
        -- monad
      , "return", ">>"
        -- alternative
      , "some", "many"
        -- foldable
      , "foldr1", "foldl1", "elem", "maximum", "minimum", "sum", "product"
        -- traversable
      , "sequenceA", "mapM", "sequence"
        -- semigroup
      , "sconcat", "stimes"
        -- monoid
      , "mconcat"
      ]


------------------------------------------------------------------------------
-- | Extract evidence from 'AbsBinds' in scope.
absBinds ::  SrcSpan -> LHsBindLR GhcTc GhcTc -> [PredType]
#if __GLASGOW_HASKELL__ >= 900
absBinds dst (L src (FunBind w _ _ _))
  | dst `isSubspanOf` src
  = wrapper w
absBinds dst (L src (AbsBinds _ _ h _ _ z _))
#else
absBinds dst (L src (AbsBinds _ _ h _ _ _ _))
#endif
  | dst `isSubspanOf` src
  = fmap idType h
#if __GLASGOW_HASKELL__ >= 900
    <> foldMap (absBinds dst) z
#endif
absBinds _ _ = []


------------------------------------------------------------------------------
-- | Extract evidence from 'HsWrapper's in scope
wrapperBinds ::  SrcSpan -> LHsExpr GhcTc -> [PredType]
#if __GLASGOW_HASKELL__ >= 900
wrapperBinds dst (L src (XExpr (WrapExpr (HsWrap h _))))
#else
wrapperBinds dst (L src (HsWrap _ h _))
#endif
  | dst `isSubspanOf` src
  = wrapper h
wrapperBinds _ _ = []


------------------------------------------------------------------------------
-- | Extract evidence from the 'ConPatOut's bound in this 'Match'.
matchBinds :: SrcSpan -> LMatch GhcTc (LHsExpr GhcTc) -> [PredType]
matchBinds dst (L src (Match _ _ pats _))
  | dst `isSubspanOf` src
  = everything (<>) (mkQ mempty patBinds) pats
matchBinds _ _ = []


------------------------------------------------------------------------------
-- | Extract evidence from a 'ConPatOut'.
patBinds ::  Pat GhcTc -> [PredType]
#if __GLASGOW_HASKELL__ >= 900
patBinds (ConPat{ pat_con_ext = ConPatTc { cpt_dicts = dicts }})
#else
patBinds (ConPatOut { pat_dicts = dicts })
#endif
  = fmap idType dicts
patBinds _ = []


------------------------------------------------------------------------------
-- | Extract the types of the evidence bindings in scope.
wrapper ::  HsWrapper -> [PredType]
wrapper (WpCompose h h2) = wrapper h <> wrapper h2
wrapper (WpEvLam v) = [idType v]
wrapper _ = []

