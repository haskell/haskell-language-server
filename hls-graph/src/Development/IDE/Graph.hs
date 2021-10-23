{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.Graph(
    shakeOptions,
    Rules,
    Action, action,
    Key(..),
    actionFinally, actionBracket, actionCatch, actionFork,
    -- * Configuration
    ShakeOptions(shakeAllowRedefineRules, shakeThreads, shakeFiles, shakeExtra),
    getShakeExtra, getShakeExtraRules, newShakeExtra,
    -- * Explicit parallelism
    parallel,
    -- * Oracle rules
    ShakeValue, RuleResult,
    -- * Special rules
    alwaysRerun,
    -- * Batching
    reschedule,
    -- * Dirty keys
    getDirtySet,
    ) where

import           Development.IDE.Graph.Database
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
