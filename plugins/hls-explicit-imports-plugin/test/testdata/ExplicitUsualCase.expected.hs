module ExplicitUsualCase where

import ExplicitA ( a1 )

main :: IO ()
main = putStrLn $ "hello " ++ a1
