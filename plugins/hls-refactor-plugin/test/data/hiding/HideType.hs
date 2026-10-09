module HideType where

import AVec (Vec, fromList)
import BVec (fromList, (++))
import CVec hiding (cons)
import DVec hiding (cons, snoc)
import EVec as E

type TheType = Vec
