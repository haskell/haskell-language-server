module Parenthesis where

e :: Int -> Int
e x = x + 1

-- the expression has a lower precedence, so must be spliced with parenthesis
f :: Int
f = 2 * (3 + 1)
