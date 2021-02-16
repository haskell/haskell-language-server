{-# LANGUAGE TemplateHaskell, UnboxedTuples #-}
module THA where
import Language.Haskell.TH

f :: Int -> (# Int, Int #)
f x = (# x , x+1 #)

th_a :: DecsQ
th_a = case f 1 of (# a , b #) -> [d| a = () |]
