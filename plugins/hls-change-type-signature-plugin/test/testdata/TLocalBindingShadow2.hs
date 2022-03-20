module TLocalBindingShadow2 where

import Control.Monad (forM)

local :: Int -> Int
local x = let test :: Int -> Int
              test = forM
          in test x

test :: String -> String
test = reverse
