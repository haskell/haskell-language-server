{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Graph.Rule(
    -- * Defining builtin rules
    -- | Functions and types for defining new types of Shake rules.
    addRule,
    Shake.RunMode(..), Shake.RunChanged(..), Shake.RunResult(..),
    -- * Calling builtin rules
    -- | Wrappers around calling Shake rules. In general these should be specialised to a builtin rule.
    apply, apply1,
    ) where

import           Development.IDE.Graph.Internal.Action
import           Development.IDE.Graph.Internal.Rules
import qualified Development.Shake.Rule                as Shake
