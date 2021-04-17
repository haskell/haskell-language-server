{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.IDE.Graph.Internal.Action where

import qualified Development.Shake as Shake
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Fail

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
