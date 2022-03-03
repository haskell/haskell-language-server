module TLocalBinding where

local :: Int -> Int
local x = let test :: [Int] -> Int
              test = head . reverse
          in x + 1
