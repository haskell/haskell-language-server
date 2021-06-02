module Main where

import A
import D
import Data.List (intercalate)

main :: IO ()
main = putStrLn 
     $ "hello " 
    <> intercalate ", " [b1, c1, e2]
