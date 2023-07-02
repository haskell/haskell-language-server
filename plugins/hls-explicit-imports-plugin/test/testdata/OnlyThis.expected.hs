module OnlyThis where

import A ( a1 )
import B

main :: IO ()
main = putStrLn $ "hello " ++ a1 ++ b1
