module UnfoldComment where

-- comments in the unfolded definition are kept
e :: Int -> Int
e x = x {- a -} + 1

f :: Int
f = 2 {- a -} + 1
