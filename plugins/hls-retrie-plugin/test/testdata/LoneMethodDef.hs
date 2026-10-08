module LoneMethodDef (e) where

import LoneMethod (method2)

e :: Int -> Int
e x = method2 (x :: Int)
