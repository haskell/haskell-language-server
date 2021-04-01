module Wingman.Auto where

import           Control.Monad.State (gets)
import           Refinery.Tactic
import           Wingman.Context
import           Wingman.Judgements
import           Wingman.KnownStrategies
import           Wingman.Machinery (tracing)
import           Wingman.Tactics
import           Wingman.Types


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

