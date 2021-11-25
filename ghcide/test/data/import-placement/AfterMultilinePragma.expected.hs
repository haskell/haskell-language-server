{-# LANGUAGE RecordWildCards, 
   OverloadedStrings #-}
import Data.Monoid
   

class Semigroup a => SomeData a

-- | a comment
instance SomeData All
