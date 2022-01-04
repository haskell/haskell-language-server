module ExportedFunction (foo) where

foo :: Num p => [a] -> p
foo [] = 0
foo xs = 1
