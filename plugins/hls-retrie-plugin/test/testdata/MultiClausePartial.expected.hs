module MultiClausePartial where

e :: Int -> Int
e 0 = 1
e n = n

-- there are no argument names to dispatch on, so bind them in a lambda
a :: [Int]
a = map (\ x -> case x of
             0 -> 1
             n -> n) [1, 2]
