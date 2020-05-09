module Main where

{-@ type Even = {v:Int | v mod 2 = 0} @-}

{-@ weAreEven :: [Even] @-}
weAreEven     = [(0-10), (0-4), 0, 2, 666]

{-@ notEven :: Even @-}
notEven = 7

{-@ isEven :: n:Nat -> {v:Bool | (v <=> (n mod 2 == 0))} @-}
isEven   :: Int -> Bool
isEven 0 = True
isEven 1 = False
isEven n = not (isEven (n-1))

{-@ evens :: n:Nat -> [Even] @-}
evens n = [i | i <- range 0 n, isEven i]

{-@ range :: lo:Int -> hi:Int -> [{v:Int | (lo <= v && v < hi)}] / [hi -lo] @-}
range lo hi
  | lo < hi   = lo : range (lo+1) hi
  | otherwise = []

{-@ shift :: [Even] -> Even -> [Even] @-}
shift xs k = [x + k | x <- xs]

{-@ double :: [Nat] -> [Even] @-}
double xs = [x + x | x <- xs]



---

notEven    :: Int
weAreEven  :: [Int]
shift      :: [Int] -> Int -> [Int]
double     :: [Int] -> [Int]
range      :: Int -> Int -> [Int]

main = putStrLn "hello"
