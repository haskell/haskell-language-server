module CrossQualified where

import qualified CrossMaster

bar = let crossfoo = CrossMaster.crossfoo in
    crossfoo * crossfoo

crossfoo :: Int
crossfoo = 7
