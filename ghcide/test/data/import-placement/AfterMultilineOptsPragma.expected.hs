{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, 
   OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wall,
  -Wno-unused-imports #-}
import Data.Monoid

   

class Semigroup a => SomeData a

-- | a comment
instance SomeData All
