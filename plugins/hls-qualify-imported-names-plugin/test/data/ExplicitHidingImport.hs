module ExplicitHidingImport where

import A
import A hiding (b)

thing1 = a
thing2 = b

