module CommentArg where

e :: Int -> Int
e x = x + 1

-- a comment inside the argument is kept
f :: Int -> Int
f x = x {- a -} + 2 + 1
