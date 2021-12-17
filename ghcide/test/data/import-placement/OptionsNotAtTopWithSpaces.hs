{-# LANGUAGE OverloadedStrings #-}


{-# LANGUAGE TupleSections #-}




class Semigroup a => SomeData a
instance SomeData All

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

addOne :: Int -> Int
addOne x = x + 1
