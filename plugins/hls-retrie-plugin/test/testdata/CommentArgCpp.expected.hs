{-# LANGUAGE CPP #-}
module CommentArgCpp where

e :: Int -> Int
e x = x + 1

#if 1
-- a comment inside the argument is kept in a CPP module
f :: Int -> Int
f x = x {- a -} + 2 + 1
#endif
