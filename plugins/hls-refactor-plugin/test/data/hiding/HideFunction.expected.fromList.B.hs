module HideFunction where

import AVec ()
import BVec (fromList, (++))
import CVec hiding (fromList, cons)
import DVec hiding (fromList, cons, snoc)
import EVec as E hiding (fromList)

theFun = fromList

theOp = (++)
