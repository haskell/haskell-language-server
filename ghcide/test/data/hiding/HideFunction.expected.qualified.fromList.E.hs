module HideFunction where

import AVec (fromList)
import BVec (fromList, (++))
import CVec hiding (cons)
import DVec hiding (cons, snoc)
import EVec as E

theFun = E.fromList

theOp = (++)
