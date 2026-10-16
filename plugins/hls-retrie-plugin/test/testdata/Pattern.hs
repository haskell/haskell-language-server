module Pattern where

-- a single clause pattern match
e (Just x) = x > 10

f = e (Just 1)
