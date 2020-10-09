module Ide.Plugin.Tactic.Auto where

import Ide.Plugin.Tactic.Context
import Ide.Plugin.Tactic.Judgements
import Ide.Plugin.Tactic.KnownStrategies
import Ide.Plugin.Tactic.Tactics
import Ide.Plugin.Tactic.Types
import Refinery.Tactic


------------------------------------------------------------------------------
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = do
  jdg <- goal
  current <- getCurrentDefinitions
  traceMX "goal" jdg
  commit
    knownStrategies
    (localTactic (auto' 4) $ disallowing $ fmap fst current)

