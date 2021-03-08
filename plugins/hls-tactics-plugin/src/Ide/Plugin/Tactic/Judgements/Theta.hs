{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Tactic.Judgements.Theta
  ( getMethodHypothesisAtHole
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Generics.SYB
import           GhcPlugins (EvVar, mkVarOcc)
import           Ide.Plugin.Tactic.Machinery
import           Ide.Plugin.Tactic.Types


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
  . (everything (<>) $ mkQ mempty $ evbinds dst)


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
      ]


------------------------------------------------------------------------------
-- | Extract the types of the evidence bindings in scope.
evbinds ::  SrcSpan -> LHsBindLR GhcTc GhcTc -> [PredType]
evbinds dst (L src (AbsBinds _ _ h _ _ _ _))
  | dst `isSubspanOf` src = fmap idType h
evbinds _ _ = []

