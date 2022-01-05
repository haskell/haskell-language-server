module MultiPositions where

import Data.List (find)
import qualified Data.Foldable (foldl, foldl')

someFunc :: Int -> String -> (Int, String)
someFunc x y =
    if x >= 0
    then (42, y)
    else (43, y)
