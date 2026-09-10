module InfixExtraArg where

e :: Int -> Int -> Int -> Int
e a b c = a + b + c

-- in infix form, the first two parameters come from the Op and the remaining
-- parameter via application
f :: Int
f = (1 `e` 2) 3
