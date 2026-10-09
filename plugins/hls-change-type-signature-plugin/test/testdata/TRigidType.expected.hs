module TRigidType where

test :: [[Int]] -> Int
test = go . head . reverse
    where
        go = head . reverse
