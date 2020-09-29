{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Tactic.Judgements where

import           Data.Char
import           Data.Coerce
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Development.IDE.Spans.LocalBindings
import           Ide.Plugin.Tactic.Types
import           OccName
import           SrcLoc
import           Type


------------------------------------------------------------------------------
-- | Given a 'SrcSpan' and a 'Bindings', create a hypothesis.
hypothesisFromBindings :: RealSrcSpan -> Bindings -> Map OccName CType
hypothesisFromBindings span bs = buildHypothesis $ getLocalScope bs span

------------------------------------------------------------------------------
-- | Convert a @Set Id@ into a hypothesis.
buildHypothesis :: [(Name, Maybe Type)] -> Map OccName CType
buildHypothesis
  = M.fromList
  . mapMaybe go
  where
    go (occName -> occ, t)
      | Just ty <- t
      , isAlpha . head . occNameString $ occ = Just (occ, CType ty)
      | otherwise = Nothing


hasDestructed :: Judgement -> OccName -> Bool
hasDestructed j n = S.member n $ _jDestructed j

destructing :: OccName -> Judgement -> Judgement
destructing n jdg@Judgement{..} = jdg
  { _jDestructed = _jDestructed <> S.singleton n
  }

withNewGoal :: a -> Judgement' a -> Judgement' a
withNewGoal t jdg = jdg
  { _jGoal = t
  }

introducing :: [(OccName, a)] -> Judgement' a -> Judgement' a
introducing ns jdg@Judgement{..} = jdg
  { _jHypothesis = M.fromList ns <> _jHypothesis
  }

------------------------------------------------------------------------------
-- | Pattern vals are currently tracked in jHypothesis, with an extra piece of data sitting around in jPatternVals.
introducingPat :: [(OccName, a)] -> Judgement' a -> Judgement' a
introducingPat ns jdg@Judgement{..} = jdg
  { _jHypothesis  = M.fromList ns <> _jHypothesis
  , _jPatternVals = S.fromList (fmap fst ns) <> _jPatternVals
  }


disallowing :: [OccName] -> Judgement' a -> Judgement' a
disallowing ns jdg@Judgement{..} = jdg
  { _jHypothesis = M.withoutKeys _jHypothesis $ S.fromList ns
  }

jHypothesis :: Judgement' a -> Map OccName a
jHypothesis = _jHypothesis


------------------------------------------------------------------------------
-- | Only the hypothesis members which are pattern vals
jPatHypothesis :: Judgement' a -> Map OccName a
jPatHypothesis jdg
  = M.restrictKeys (jHypothesis jdg) $ _jPatternVals jdg


jGoal :: Judgement' a -> a
jGoal = _jGoal


substJdg :: TCvSubst -> Judgement -> Judgement
substJdg subst = fmap $ coerce . substTy subst . coerce

mkFirstJudgement :: M.Map OccName CType -> Type -> Judgement' CType
mkFirstJudgement hy = Judgement hy mempty mempty . CType

