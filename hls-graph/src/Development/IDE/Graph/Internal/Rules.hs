{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.Graph.Internal.Rules where

import qualified Development.Shake as Shake
import qualified Development.Shake.Rule as Shake
import Development.Shake.Classes
import Development.IDE.Graph.Internal.Action
import Control.Monad.IO.Class
import Control.Monad.Fail
import qualified Data.ByteString as BS

newtype Rules a = Rules {fromRules :: Shake.Rules a}
    deriving (Monoid, Semigroup, Monad, Applicative, Functor, MonadIO, MonadFail)

action :: Action a -> Rules ()
action = Rules . Shake.action . fromAction

addRule
    :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value, NFData value, Show value)
    => (key -> Maybe BS.ByteString -> Shake.RunMode -> Action (Shake.RunResult value))
    -> Rules ()
addRule f = Rules $ Shake.addBuiltinRule Shake.noLint Shake.noIdentity $ \k bs r -> fromAction $ f k bs r
