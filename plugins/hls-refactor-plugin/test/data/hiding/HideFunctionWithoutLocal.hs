module HideFunctionWithoutLocal where

import AVec (fromList)
import BVec (fromList, (++))
import CVec hiding (cons)
import DVec hiding (cons, snoc)
import EVec as E

theOp = (++)

data Vec a

(++) = undefined
