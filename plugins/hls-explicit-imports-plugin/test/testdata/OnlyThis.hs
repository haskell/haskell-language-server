module OnlyThis where

import A
import B

main :: IO ()
main = putStrLn $ "hello " ++ a1 ++ b1
