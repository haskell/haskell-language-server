{-# LANGUAGE CPP #-}
module CppStraddle where

e :: Int -> Int
e x = x + 1

-- a rewrite spanning a directive is refused, /please/ don't
-- write code like this
f :: Int -> Int
f x = e
#if 1
    x
#else
    2
#endif
