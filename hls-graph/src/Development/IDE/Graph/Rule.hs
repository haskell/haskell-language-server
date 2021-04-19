
module Development.IDE.Graph.Rule(
    -- * Defining builtin rules
    -- | Functions and types for defining new types of Shake rules.
    addBuiltinRule,
    BuiltinLint, noLint, BuiltinIdentity, noIdentity, BuiltinRun, RunMode(..), RunChanged(..), RunResult(..),
    -- * Calling builtin rules
    -- | Wrappers around calling Shake rules. In general these should be specialised to a builtin rule.
    apply, apply1,
    ) where

import Development.Shake.Rule
