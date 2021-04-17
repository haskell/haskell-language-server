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

import qualified Data.ByteString as BS
import qualified Development.Shake as Shake
import qualified Development.Shake.Rule as Shake
import Development.Shake.Classes
import Development.IDE.Graph.Internal.Action
import Development.IDE.Graph.Internal.Rules

addRule
    :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value, NFData value, Show value)
    => (key -> Maybe BS.ByteString -> Shake.RunMode -> Action (Shake.RunResult value))
    -> Rules ()
addRule f = Rules $ Shake.addBuiltinRule Shake.noLint Shake.noIdentity $ \k bs r -> fromAction $ f k bs r


apply1 :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value) => key -> Action value
apply1 = Action . Shake.apply1

apply :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value) => [key] -> Action [value]
apply = Action . Shake.apply
