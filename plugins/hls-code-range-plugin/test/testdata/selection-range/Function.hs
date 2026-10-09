module FuncMultiMatch where

someFunc :: Integral a => a -> String -> Maybe (Int, String)
someFunc _ "magic" = Nothing
someFunc x y = Just (fromIntegral x, y)
  where
    go :: Int -> Int
    go 0 = -1
    go x = x + 1

    hi = "greeting"

otherFunc :: String -> String
otherFunc = id
