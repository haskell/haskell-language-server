module ExplicitHidingImport where

import A
import A hiding (b)

thing1 = A.a
thing2 = b

