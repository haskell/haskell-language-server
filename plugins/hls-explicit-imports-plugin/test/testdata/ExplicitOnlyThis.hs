module ExplicitOnlyThis where

import ExplicitA
import ExplicitB

main :: IO ()
main = putStrLn $ "hello " ++ a1 ++ b1
