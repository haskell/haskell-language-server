module TExpectedActual where

fullSig :: Int -> Int
fullSig = go
    where
        go = head . reverse
