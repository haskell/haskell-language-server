{-# LANGUAGE CPP #-}
module CPP where

-- allow rewrites into cpp branches
e :: Int -> Int
e x = x + 1

#if 1
f :: Int -> Int
f x = 2 + (x + 1)
#else
f :: Int -> Int
f x = 3 + (x + 1)
#endif
