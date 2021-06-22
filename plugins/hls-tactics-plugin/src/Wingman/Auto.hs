
module Wingman.Auto where

import           Control.Monad.Reader.Class (asks)
import           Control.Monad.State (gets)
import qualified Data.Set as S
import           Refinery.Tactic
import           Wingman.Judgements
import           Wingman.KnownStrategies
import           Wingman.Machinery (tracing, getCurrentDefinitions)
import           Wingman.Tactics
import           Wingman.Types


------------------------------------------------------------------------------
-- | Automatically solve a goal.
auto :: TacticsM ()
auto = do
  jdg <- goal
  skolems <- gets ts_skolems
  gas <- asks $ cfg_auto_gas . ctxConfig
  current <- getCurrentDefinitions
  traceMX "goal" jdg
  traceMX "ctx" current
  traceMX "skolems" skolems
  commit knownStrategies
    . tracing "auto"
    . localTactic (auto' gas)
    . disallowing RecursiveCall
    . S.fromList
    $ fmap fst current

