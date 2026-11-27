module UnfoldComment where

-- comments in the unfolded definition are kept
e :: Int -> Int
e x = x {- a -} + 1

f :: Int
f = e 2
