{-# LANGUAGE ViewPatterns #-}

module Wingman.Judgements.Theta
  ( getMethodHypothesisAtHole
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Generics.SYB
import           GhcPlugins (mkVarOcc)
import           TcEvidence
import           Wingman.Machinery
import           Wingman.Types


------------------------------------------------------------------------------
-- | Create a 'Hypothesis' containing 'ClassMethodPrv' provenance. For every
-- dictionary that is in scope at the given 'SrcSpan', find every method and
-- superclass method available.
getMethodHypothesisAtHole :: SrcSpan -> LHsBinds GhcTc -> Hypothesis CType
getMethodHypothesisAtHole dst
  = Hypothesis
  . excludeForbiddenMethods
  . fromMaybe []
  . foldMap methodHypothesis
  . (everything (<>) $
      mkQ mempty (absBinds dst) `extQ` wrapperBinds dst `extQ` matchBinds dst)


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

