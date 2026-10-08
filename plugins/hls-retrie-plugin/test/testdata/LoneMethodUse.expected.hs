module LoneMethodUse where

import LoneMethodDef (e)
import LoneMethod (method2)

f :: Int
f = method2 (7 :: Int)
