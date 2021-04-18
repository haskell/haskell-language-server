{-# LANGUAGE PatternSynonyms #-}

module Development.IDE.Graph(
    shakeOptions,
    Rules,
    Action, action,
    actionFinally, actionBracket, actionCatch,
    ShakeException(..),
    -- * Configuration
    ShakeOptions(shakeThreads, shakeFiles, shakeExtra),
    getShakeExtra, getShakeExtraRules, addShakeExtra,
    -- * Explicit parallelism
    parallel,
    -- * Oracle rules
    ShakeValue, RuleResult,
    -- * Special rules
    alwaysRerun,
    -- * Batching
    reschedule,
    ) where

import Development.Shake
