module ConstructorUse where

import ConstructorDef (T, e)
import ConstructorDef (T (MkT))

f :: T
f = MkT 7
