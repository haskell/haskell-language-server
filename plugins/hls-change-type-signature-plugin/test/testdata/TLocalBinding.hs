module TLocalBinding where

import Control.Monad (forM)

local :: Int -> Int
local x = let test :: Int -> Int
              test = forM
          in x + 1
