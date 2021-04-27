module Main where

import B ( b1 )
import C ( c1 )
import D
import Data.List (intercalate)

main :: IO ()
main = putStrLn 
     $ "hello " 
    <> intercalate ", " [b1, c1, e1]
