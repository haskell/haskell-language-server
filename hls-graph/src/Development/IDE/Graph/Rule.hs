{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Graph.Rule(
    -- * Defining builtin rules
    -- | Functions and types for defining new types of Shake rules.
    addRule,
    RunMode(..), RunChanged(..), RunResult(..),
    -- * Calling builtin rules
    -- | Wrappers around calling Shake rules. In general these should be specialised to a builtin rule.
    apply, apply1,
    ) where

import qualified Data.ByteString as BS
import Development.Shake
import Development.Shake.Rule
import Development.Shake.Classes

addRule
    :: (RuleResult key ~ value, ShakeValue key, Typeable value, NFData value, Show value)
    => (key -> Maybe BS.ByteString -> RunMode -> Action (RunResult value))
    -> Rules ()
addRule = addBuiltinRule noLint noIdentity
