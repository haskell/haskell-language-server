module Main where

import qualified RefineA as RA
import RefineD
import Data.List (intercalate)

main :: IO ()
main = putStrLn 
     $ "hello " 
    <> intercalate ", " [RA.b1, RA.c1, e2]
