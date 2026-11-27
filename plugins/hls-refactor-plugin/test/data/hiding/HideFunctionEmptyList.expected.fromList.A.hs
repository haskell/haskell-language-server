module HideFunctionEmptyList where

import Prelude hiding ((++))
import AVec
import BVec hiding (fromList)
import CVec hiding (fromList )
import DVec
  hiding (fromList)

theFun = fromList

theOp = (++)
