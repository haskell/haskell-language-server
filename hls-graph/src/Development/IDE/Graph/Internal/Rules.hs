{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.IDE.Graph.Internal.Rules where

import qualified Development.Shake as Shake
import Development.IDE.Graph.Internal.Action
import Control.Monad.IO.Class
import Control.Monad.Fail

newtype Rules a = Rules {fromRules :: Shake.Rules a}
    deriving (Monoid, Semigroup, Monad, Applicative, Functor, MonadIO, MonadFail)

action :: Action a -> Rules ()
action = Rules . Shake.action . fromAction
