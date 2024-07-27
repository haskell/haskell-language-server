module Main where

import RefineA
import RefineD
import RefineF
import Data.List (intercalate)

main :: IO ()
main = putStrLn 
     $ "hello " 
    <> intercalate ", " [b1, c1, e1, f1, g1]
