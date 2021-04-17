
module Development.IDE.Graph.Rule(
    -- * Defining builtin rules
    -- | Functions and types for defining new types of Shake rules.
    addBuiltinRule,
    BuiltinLint, noLint, BuiltinIdentity, noIdentity, BuiltinRun, RunMode(..), RunChanged(..), RunResult(..),
    -- * Calling builtin rules
    -- | Wrappers around calling Shake rules. In general these should be specialised to a builtin rule.
    apply, apply1,
    -- * User rules
    -- | Define user rules that can be used by builtin rules.
    --   Absent any builtin rule making use of a user rule at a given type, a user rule will have on effect -
    --   they have no inherent effect or interpretation on their own.
    addUserRule, getUserRuleList, getUserRuleMaybe, getUserRuleOne,
    -- * Lint integration
    -- | Provide lint warnings when running code.
    lintTrackRead, lintTrackWrite, lintTrackAllow,
    -- * History caching
    -- | Interact with the non-local cache. When using the cache it is important that all
    --   rules have accurate 'BuiltinIdentity' functions.
    historyIsEnabled, historySave, historyLoad
    ) where

import Development.Shake.Rule
