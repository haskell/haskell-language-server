{-# LANGUAGE CPP #-}
module CPP where

-- only the active cpp branch is rewritten
e :: Int -> Int
e x = x + 1

#if 1
f :: Int -> Int
f x = 2 + e x
#else
f :: Int -> Int
f x = 3 + e x
#endif
