{-# LANGUAGE PatternSynonyms #-}
module Development.IDE.Graph(
      shakeOptions,
    Rules,
    Action, action,
    pattern Key,
    newKey, renderKey,
    actionFinally, actionBracket, actionCatch, actionFork,
    -- * Configuration
    ShakeOptions(shakeAllowRedefineRules, shakeExtra),
    getShakeExtra, getShakeExtraRules, newShakeExtra,
    -- * Explicit parallelism
    parallel,
    -- * Oracle rules
    ShakeValue, RuleResult,
    -- * Special rules
    alwaysRerun,
    -- * Actions for inspecting the keys in the database
    getDirtySet,
    getKeysAndVisitedAge,
    module Development.IDE.Graph.KeyMap,
    module Development.IDE.Graph.KeySet,
    ) where

import           Development.IDE.Graph.Database
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.KeyMap
import           Development.IDE.Graph.KeySet
