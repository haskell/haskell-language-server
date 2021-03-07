{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Tactic.Judgements.Theta where

import Data.Maybe (fromMaybe)
import Development.IDE.GHC.Compat
import Generics.SYB
import GhcPlugins (EvVar)
import Ide.Plugin.Tactic.Machinery
import Ide.Plugin.Tactic.Types


getMethodHypothesisAtHole :: Data a => SrcSpan -> a -> Hypothesis CType
getMethodHypothesisAtHole dst
  = Hypothesis
  . fromMaybe []
  . foldMap methodHypothesis
  . getEvidenceAtHole dst


getEvidenceAtHole :: Data a => SrcSpan -> a -> [PredType]
getEvidenceAtHole dst a = everything (<>) (mkQ mempty $ evbinds dst) a


evbinds ::  SrcSpan -> LHsBindLR GhcTc GhcTc -> [PredType]
evbinds dst (L src (AbsBinds _ _ h _ _ _ _))
  | dst `isSubspanOf` src = fmap idType h
evbinds _ _ = []

