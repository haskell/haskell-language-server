module Guards where

-- inline a function that uses guards
e :: Int -> String
e x
  | x > 0 = "Positive"
  | otherwise = "Negative or Zero"

f :: String
f = case 6 of
  x
    | x > 0 -> "Positive"
    | otherwise -> "Negative or Zero"

