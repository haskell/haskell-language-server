module Guards where

-- inline a function that uses guards
e :: Int -> String
e x
  | x > 0 = "Positive"
  | otherwise = "Negative or Zero"

f :: String
f = e 6

