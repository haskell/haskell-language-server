{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Development.IDE.Graph.Internal.Action where

import           Control.Exception
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Development.Shake         as Shake
import           Development.Shake.Classes
import qualified Development.Shake.Rule    as Shake

newtype Action a = Action {fromAction :: Shake.Action a}
    deriving (Monad, Applicative, Functor, MonadIO, MonadFail)

alwaysRerun :: Action ()
alwaysRerun = Action Shake.alwaysRerun

reschedule :: Double -> Action ()
reschedule = Action . Shake.reschedule

parallel :: [Action a] -> Action [a]
parallel = Action . Shake.parallel . map fromAction

actionCatch :: Exception e => Action a -> (e -> Action a) -> Action a
actionCatch a b = Action $ Shake.actionCatch (fromAction a) (fromAction . b)

actionBracket :: IO a -> (a -> IO b) -> (a -> Action c) -> Action c
actionBracket a b c = Action $ Shake.actionBracket a b (fromAction . c)

actionFinally :: Action a -> IO b -> Action a
actionFinally a b = Action $ Shake.actionFinally (fromAction a) b

apply1 :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value) => key -> Action value
apply1 = Action . Shake.apply1

apply :: (Shake.RuleResult key ~ value, Shake.ShakeValue key, Typeable value) => [key] -> Action [value]
apply = Action . Shake.apply
