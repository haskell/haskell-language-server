module ExplicitImport where

import A (a)
import A (b)

thing1 = a
thing2 = A.b

