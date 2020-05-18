module Main where

main = putStrLn "hello"

foo :: Int
foo = bb

bb = 5

baz = do
  putStrLn "hello"

f x = x+1