module TLocalBindingShadow2 where

local :: Int -> Int
local x = let test :: Int -> Int
              test = head . reverse
          in test x

test :: String -> String
test = reverse
