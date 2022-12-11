module Nest where

f :: Int
f = g
    where
        g :: Int
        g = h
        h :: Int
        h = k where k :: Int
                    k = 3
