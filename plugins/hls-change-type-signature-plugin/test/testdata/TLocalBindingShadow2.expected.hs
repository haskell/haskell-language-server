module TLocalBindingShadow2 where

import Control.Monad (forM)

local :: Int -> Int
local x = let test :: t0 a0 -> (a0 -> m0 b0) -> m0 (t0 b0)
              test = forM
          in test x

test :: String -> String
test = reverse
