module FieldClash where

import FieldClashDef (R (R), e)

-- a local field occupies the selector's spelling: splicing the body
-- would leave an ambiguous occurrence, so the site must be refused
data S = S { name :: Int }

f :: String
f = e (R "x" 1)
