{-# LANGUAGE LinearTypes #-}
module ConstructorsLinear where

data A = A
data B = B Int Word Bool
data C = C ![Int] {-# UNPACK #-} !Bool
data D = D { da :: Int, db :: Bool }
data E = E { ea :: !Int, eb :: {-# UNPACK #-} ![Bool] }
newtype F = F Int
