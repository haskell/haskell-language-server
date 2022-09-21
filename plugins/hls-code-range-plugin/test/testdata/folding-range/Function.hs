{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use module export list" #-}
module Function(isEven) where

isEven :: Integer -> Bool
isEven n = if n `mod` 2 == 0
           then True
           else False

isOdd :: Integer -> Bool
isOdd n = if n `mod` 2 == 0
           then False
           else True
