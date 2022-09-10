{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use module export list" #-}
module FuncMultiMatch where

isEven :: Integer -> Bool
isEven n = if n `mod` 2 == 0
           then True
           else False
