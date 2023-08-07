module ExplicitExported (module ExplicitA) where

import ExplicitA

main :: IO ()
main = putStrLn $ "hello " ++ a1
