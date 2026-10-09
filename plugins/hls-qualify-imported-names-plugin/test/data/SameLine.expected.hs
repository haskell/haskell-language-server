module SameLine where

import A

thing = ((A.a) . (A.a) . (A.a)) (1 `A.op` 2 `A.op` 3 `A.op` 4)
