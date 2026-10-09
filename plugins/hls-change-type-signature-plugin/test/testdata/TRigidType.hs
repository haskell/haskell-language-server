module TRigidType where

test :: a -> Int
test = go . head . reverse
    where
        go = head . reverse
