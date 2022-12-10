module HideType where

import AVec (Vec, fromList)
import BVec (fromList, (++))
import CVec hiding (Vec, cons)
import DVec hiding (Vec, cons, snoc)
import EVec as E hiding (Vec)

type TheType = Vec
