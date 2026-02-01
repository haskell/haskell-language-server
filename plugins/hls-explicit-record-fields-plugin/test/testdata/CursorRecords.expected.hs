{-# LANGUAGE RecordWildCards #-}
module CursorRecords where

data L4 = L4{ a0 :: Int}
data L3 = L3{ a1 :: Int, a11 :: Int, l4 :: L4}
data L2 = L2{ a2 :: Int, l3 :: L3}
data L1 = L1{ a3 :: Int, l2 :: L2}

test :: L1 -> Int
test L1 {l2 = L2{ l3 = L3 {l4 = L4 {..}, a1, a11}, ..}, ..} =
    a0 + a1 + a2 + a3 + a11
