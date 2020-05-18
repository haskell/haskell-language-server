module Main where

main = putStrLn "hello"

foo :: Int
foo = bb

bb = 5

bug -- no hlint returned because of this, despite redundant do below

baz = do
  putStrLn "hello"

f x = x+1
