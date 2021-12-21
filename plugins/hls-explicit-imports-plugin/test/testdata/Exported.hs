module Exported (module A) where

import A

main :: IO ()
main = putStrLn $ "hello " ++ a1
