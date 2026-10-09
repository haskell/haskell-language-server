module Control.Concurrent.Strict
    (modifyVar', modifyVarIO'
    ,modifyVar, modifyVar_
    ,module Control.Concurrent.Extra
    ) where

import           Control.Concurrent.Extra hiding (modifyVar, modifyVar',
                                           modifyVar_)
import qualified Control.Concurrent.Extra as Extra
import           Control.Exception        (evaluate)
import           Control.Monad            (void)
import           Data.Tuple.Extra         (dupe)

-- | Strict modification that returns the new value
modifyVar' :: Extra.Var a -> (a -> a) -> IO a
modifyVar' var upd = modifyVarIO' var (pure . upd)

-- | Strict modification that returns the new value
modifyVarIO' :: Extra.Var a -> (a -> IO a) -> IO a
modifyVarIO' var upd = do
    res <- Extra.modifyVar var $ \v -> do
        v' <- upd v
        pure $ dupe v'
    evaluate res

modifyVar :: Extra.Var a -> (a -> IO (a, b)) -> IO b
modifyVar var upd = do
    (new, res) <- Extra.modifyVar var $ \old -> do
        (new,res) <- upd old
        return (new, (new, res))
    void $ evaluate new
    return res

modifyVar_ :: Extra.Var a -> (a -> IO a) -> IO ()
modifyVar_ var upd = void $ modifyVarIO' var upd
