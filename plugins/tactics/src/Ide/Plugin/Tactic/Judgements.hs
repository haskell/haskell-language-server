{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Tactic.Judgements where

import           Data.Coerce
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Ide.Plugin.Tactic.Types
import           Type


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

jHypothesis :: Judgement' a -> Map OccName a
jHypothesis = _jHypothesis

jGoal :: Judgement' a -> a
jGoal = _jGoal


substJdg :: TCvSubst -> Judgement -> Judgement
substJdg subst = fmap $ coerce . substTy subst . coerce

