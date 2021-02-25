module Ide.Plugin.Tactic.Auto where

import           Control.Monad.State               (gets)
import           Ide.Plugin.Tactic.Context
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.KnownStrategies
import           Ide.Plugin.Tactic.Machinery       (tracing)
import           Ide.Plugin.Tactic.Tactics
import           Ide.Plugin.Tactic.Types
import           Refinery.Tactic


------------------------------------------------------------------------------
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = do
  jdg <- goal
  skolems <- gets ts_skolems
  current <- getCurrentDefinitions
  traceMX "goal" jdg
  traceMX "ctx" current
  traceMX "skolems" skolems
  commit knownStrategies
    . tracing "auto"
    . localTactic (auto' 4)
    . disallowing RecursiveCall
    $ fmap fst current

