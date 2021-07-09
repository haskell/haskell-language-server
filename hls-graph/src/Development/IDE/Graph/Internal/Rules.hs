{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Development.IDE.Graph.Internal.Rules where

import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.ByteString                       as BS
import           Development.IDE.Graph.Internal.Action
import qualified Development.Shake                     as Shake
import           Development.Shake.Classes
import qualified Development.Shake.Rule                as Shake

newtype Rules a = Rules {fromRules :: Shake.Rules a}
    deriving (Monoid, Semigroup, Monad, Applicative, Functor, MonadIO, MonadFail)

action :: Action a -> Rules ()
action = Rules . Shake.action . fromAction

addRule
    :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value, NFData value, Show value)
    => (key -> Maybe BS.ByteString -> Shake.RunMode -> Action (Shake.RunResult value))
    -> Rules ()
addRule f = Rules $ Shake.addBuiltinRule Shake.noLint Shake.noIdentity $ \k bs r -> fromAction $ f k bs r
