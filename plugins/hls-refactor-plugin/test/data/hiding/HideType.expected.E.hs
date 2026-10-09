module HideType where

import AVec ( fromList)
import BVec (fromList, (++))
import CVec hiding (Vec, cons)
import DVec hiding (Vec, cons, snoc)
import EVec as E

type TheType = Vec
