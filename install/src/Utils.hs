{-# LANGUAGE CPP #-}
module Utils where

import System.Info

-- | The extension of executables, @\"exe\"@ on Windows and @\"\"@ otherwise.
exe :: String
exe = if isWindows then "exe" else ""

isWindows :: Bool
#if defined(mingw32_HOST_OS)
isWindows = True
#else
isWindows = False
#endif

-- | A version of 'mapMaybe' that works with a monadic predicate.
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
{-# INLINE mapMaybeM #-}
mapMaybeM op = foldr f (pure [])
    where f x xs = do x <- op x; case x of Nothing -> xs; Just x -> do xs <- xs; pure $ x:xs