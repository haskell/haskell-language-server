module ConPatNoDisamb where

import ConPatDef (R (R), e)

-- without DisambiguateRecordFields a pattern label resolves by
-- spelling, and a local field occupies it: the site must be refused
data S = S { name :: Int }

f :: String
f = e (R "x" 1)
