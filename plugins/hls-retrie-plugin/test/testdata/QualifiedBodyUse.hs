module QualifiedBodyUse where

import QualifiedBodyDef (e)

f :: Int
f = e (Just 7)
