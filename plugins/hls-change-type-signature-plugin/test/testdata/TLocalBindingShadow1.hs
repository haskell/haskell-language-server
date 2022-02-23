module TLocalBindingShadow1 where

local :: Int -> Int
local x = let test :: Int -> Int
              test = (+2)
          in test x

test :: Int -> Double
test = head . reverse
