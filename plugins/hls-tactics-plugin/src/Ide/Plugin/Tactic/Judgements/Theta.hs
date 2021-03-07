{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Tactic.Judgements.Theta
  ( getMethodHypothesisAtHole
  ) where

import Data.Maybe (fromMaybe)
import Development.IDE.GHC.Compat
import Generics.SYB
import GhcPlugins (EvVar)
import Ide.Plugin.Tactic.Machinery
import Ide.Plugin.Tactic.Types


------------------------------------------------------------------------------
-- | Create a 'Hypothesis' containing 'ClassMethodPrv' provenance. For every
-- dictionary that is in scope at the given 'SrcSpan', find every method and
-- superclass method available.
getMethodHypothesisAtHole :: SrcSpan -> LHsBinds GhcTc -> Hypothesis CType
getMethodHypothesisAtHole dst
  = Hypothesis
  . fromMaybe []
  . foldMap methodHypothesis
  . (everything (<>) $ mkQ mempty $ evbinds dst)


------------------------------------------------------------------------------
-- | Extract the types of the evidence bindings in scope.
evbinds ::  SrcSpan -> LHsBindLR GhcTc GhcTc -> [PredType]
evbinds dst (L src (AbsBinds _ _ h _ _ _ _))
  | dst `isSubspanOf` src = fmap idType h
evbinds _ _ = []

