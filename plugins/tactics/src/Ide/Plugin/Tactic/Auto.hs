module Ide.Plugin.Tactic.Auto where

import Control.Applicative
import Ide.Plugin.Tactic.Context
import Ide.Plugin.Tactic.Judgements
import Ide.Plugin.Tactic.KnownStrategies
import Ide.Plugin.Tactic.Tactics
import Ide.Plugin.Tactic.Types


------------------------------------------------------------------------------
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = do
  current <- getCurrentDefinitions
  knownStrategies
    <|> (localTactic (auto' 4) $ disallowing $ fmap fst current)

