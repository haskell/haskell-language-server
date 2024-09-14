{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE TupleSections #-}




class Semigroup a => SomeData a
instance SomeData All

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Monoid

addOne :: Int -> Int
addOne x = x + 1
