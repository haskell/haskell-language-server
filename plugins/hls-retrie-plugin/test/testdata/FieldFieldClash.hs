module FieldFieldClash where

import FieldClashDef (R (..), e)

-- R's field is in scope, but a local field contests the spelling: the
-- site must be refused
data S = S { name :: Int }

f :: String
f = e (R "x" 1)
