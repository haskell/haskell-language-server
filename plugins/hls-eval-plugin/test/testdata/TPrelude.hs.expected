-- Prelude has no special treatment, it is imported as stated in the module.
module TPrelude where

import           Prelude hiding (foldr)

-- >>> foldr (+) 10 [2,3,5]
-- 20
foldr :: (a -> z -> z) -> z -> [a] -> z
foldr f z bs =
   (foldl (\g a -> g . f a) id bs) z

