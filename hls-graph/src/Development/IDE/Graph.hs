{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.Graph(
    shakeOptions,
    Rules,
    Action, action,
    Key(..),
    actionFinally, actionBracket, actionCatch, actionFork,
    Shake.ShakeException(..),
    -- * Configuration
    ShakeOptions(shakeAllowRedefineRules, shakeThreads, shakeFiles, shakeExtra),
    getShakeExtra, getShakeExtraRules, newShakeExtra,
    -- * Explicit parallelism
    parallel,
    -- * Oracle rules
    ShakeValue, Shake.RuleResult,
    -- * Special rules
    alwaysRerun,
    -- * Batching
    reschedule,
    ) where

import           Development.IDE.Graph.Database
import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Options
import           Development.IDE.Graph.Internal.Rules
import           Development.IDE.Graph.Internal.Types
import qualified Development.Shake                      as Shake
