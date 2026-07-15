module CrossQualified where

import qualified CrossMaster

bar = let crossfoo = CrossMaster.newFoo in
    crossfoo * crossfoo

crossfoo :: Int
crossfoo = 7
