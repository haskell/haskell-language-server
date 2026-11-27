module VarClash where

import FieldClashDef (R (..), e)

-- R's field is in scope, but a local variable contests the spelling:
-- the site must be refused
name :: Int
name = 0

f :: String
f = e (R "x" 1)
