{-# LANGUAGE ViewPatterns #-}

module Wingman.Judgements.Theta
  ( Evidence
  , getEvidenceAtHole
  , mkEvidence
  , evidenceToSubst
  , evidenceToHypothesis
  ) where

import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Generics.SYB
import           GhcPlugins (mkVarOcc, splitTyConApp_maybe, eqTyCon, getTyVar_maybe)
import           TcEvidence
import           TcType (tcTyConAppTyCon_maybe)
import           TysPrim (eqPrimTyCon)
import           Wingman.Machinery
import           Wingman.Types


data Evidence
  = EqualityOfTypes Type Type
  | HasInstance PredType
  deriving (Show)


mkEvidence :: PredType -> Maybe Evidence
mkEvidence (getEqualityTheta -> Just (a, b))
  = Just $ EqualityOfTypes a b
mkEvidence inst@(tcTyConAppTyCon_maybe -> Just (isClassTyCon -> True))
  = Just $ HasInstance inst
mkEvidence _ = Nothing


getEvidenceAtHole :: SrcSpan -> LHsBinds GhcTc -> [Evidence]
getEvidenceAtHole dst
  = mapMaybe mkEvidence
  . (everything (<>) $ mkQ mempty (absBinds dst) `extQ` wrapperBinds dst `extQ` matchBinds dst)


evidenceToSubst :: Evidence -> TacticState -> TacticState
evidenceToSubst (EqualityOfTypes a b) ts =
  let tyvars = S.fromList $ mapMaybe getTyVar_maybe [a, b]
      -- If we can unify our skolems, at least one is no longer a skolem.
      -- Removing them from this set ensures we can get a subtitution between
      -- the two. But it's okay to leave them in 'ts_skolems' in general, since
      -- they won't exist after running this substitution.
      skolems = ts_skolems ts S.\\ tyvars
   in
  case tryUnifyUnivarsButNotSkolems skolems (CType a) (CType b) of
    Just subst -> updateSubst subst ts
    Nothing -> ts
evidenceToSubst HasInstance{} ts = ts


evidenceToHypothesis :: Evidence -> Hypothesis CType
evidenceToHypothesis EqualityOfTypes{} = mempty
evidenceToHypothesis (HasInstance t) =
  Hypothesis . excludeForbiddenMethods . fromMaybe [] $ methodHypothesis t


------------------------------------------------------------------------------
-- | Given @a ~ b@, returns @Just (a, b)@, otherwise @Nothing@.
getEqualityTheta :: PredType -> Maybe (Type, Type)
getEqualityTheta (splitTyConApp_maybe -> Just (tc, [_k, a, b]))
  | tc == eqTyCon = Just (a, b)
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
      , "showsPrec"
      , "showList"
      ]


------------------------------------------------------------------------------
-- | Extract evidence from 'AbsBinds' in scope.
absBinds ::  SrcSpan -> LHsBindLR GhcTc GhcTc -> [PredType]
absBinds dst (L src (AbsBinds _ _ h _ _ _ _))
  | dst `isSubspanOf` src = fmap idType h
absBinds _ _ = []


------------------------------------------------------------------------------
-- | Extract evidence from 'HsWrapper's in scope
wrapperBinds ::  SrcSpan -> LHsExpr GhcTc -> [PredType]
wrapperBinds dst (L src (HsWrap _ h _))
  | dst `isSubspanOf` src = wrapper h
wrapperBinds _ _ = []


matchBinds :: SrcSpan -> LMatch GhcTc (LHsExpr GhcTc) -> [PredType]
matchBinds dst (L src (Match _ _ pats _))
  | dst `isSubspanOf` src = everything (<>) (mkQ mempty patBinds) pats
matchBinds _ _ = []

patBinds ::  Pat GhcTc -> [PredType]
patBinds (ConPatOut { pat_dicts = dicts })
  = fmap idType dicts
patBinds _ = []




------------------------------------------------------------------------------
-- | Extract the types of the evidence bindings in scope.
wrapper ::  HsWrapper -> [PredType]
wrapper (WpCompose h h2) = wrapper h <> wrapper h2
wrapper (WpEvLam v) = [idType v]
wrapper _ = []

