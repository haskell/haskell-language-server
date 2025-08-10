module Main where

import Data.List -- Intentionally unused import, used in the testcase

main :: IO ()
main = foo

-- Missing signature
foo = putStrLn "Hello, World"
