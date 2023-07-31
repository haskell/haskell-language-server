module ExplicitOnlyThis where

import ExplicitA ( a1 )
import ExplicitB

main :: IO ()
main = putStrLn $ "hello " ++ a1 ++ b1
