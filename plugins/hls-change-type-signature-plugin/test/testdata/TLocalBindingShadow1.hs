module TLocalBindingShadow1 where

import Control.Monad (forM)

local :: Int -> Int
local x = let test :: Int -> Int
              test = (+2)
          in test x

test :: [Double] -> Double
test = forM
