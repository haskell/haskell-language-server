module Main where

import RefineA
import RefineE ( e2 )
import Data.List (intercalate)

main :: IO ()
main = putStrLn 
     $ "hello " 
    <> intercalate ", " [b1, c1, e2]
