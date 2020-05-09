{-# OPTIONS_GHC -Wall #-}
module UnusedTerm () where
imUnused :: Int -> Int
imUnused 1 = 1
imUnused 2 = 2
imUnused _ = 3
