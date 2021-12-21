module TLocalImport where

import qualified Util

-- >>> Util.tst 11 11
-- True

tst' :: Eq a => a -> a -> Bool
tst' = Util.tst
