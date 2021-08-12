module ExportedFunction (quux) where

quux :: Num p => [a] -> p
quux [] = 0
quux xs = 1
