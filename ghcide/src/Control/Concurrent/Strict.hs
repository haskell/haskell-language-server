module Control.Concurrent.Strict
    (modifyVar', modifyVarIO'
    ,modifyVar, modifyVar_
    ,module Control.Concurrent.Extra
    ) where

import Control.Concurrent.Extra hiding (modifyVar, modifyVar_)
import qualified Control.Concurrent.Extra as Extra
import Control.Exception (evaluate)
import Data.Tuple.Extra (dupe)
import Control.Monad (void)

-- | Strict modification that returns the new value
modifyVar' :: Var a -> (a -> a) -> IO a
modifyVar' var upd = modifyVarIO' var (pure . upd)

-- | Strict modification that returns the new value
modifyVarIO' :: Var a -> (a -> IO a) -> IO a
modifyVarIO' var upd = do
    res <- Extra.modifyVar var $ \v -> do
        v' <- upd v
        pure $ dupe v'
    evaluate res

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar var upd = do
    (new, res) <- Extra.modifyVar var $ \old -> do
        (new,res) <- upd old
        return (new, (new, res))
    void $ evaluate new
    return res

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ var upd = void $ modifyVarIO' var upd
