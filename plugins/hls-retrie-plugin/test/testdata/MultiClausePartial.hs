module MultiClausePartial where

e :: Int -> Int
e 0 = 1
e n = n

-- there are no argument names to dispatch on, so bind them in a lambda
a :: [Int]
a = map e [1, 2]
