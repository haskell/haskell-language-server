{-# LANGUAGE RecordWildCards #-}
module WildcardCon where

data R = R { a :: Int, b :: Int }

-- the construction names the parameters through the wildcard
e :: Int -> Int -> R
e a b = R {..}

f :: R
f = e 1 2
