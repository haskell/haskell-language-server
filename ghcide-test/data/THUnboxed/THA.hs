{-# LANGUAGE TemplateHaskell, UnboxedTuples, BangPatterns #-}
module THA where
import Language.Haskell.TH

data Foo = Foo !Int !Char !String
  deriving Show

newtype Bar = Bar Int
  deriving Show


f :: Int -> (# Int, Int, Foo, Bar#)
f x = (# x , x+1 , Foo x 'a' "test", Bar 1 #)

th_a :: DecsQ
th_a = case f 1 of (# a , b, Foo _ _ _, Bar !_ #) -> [d| a = () |]
