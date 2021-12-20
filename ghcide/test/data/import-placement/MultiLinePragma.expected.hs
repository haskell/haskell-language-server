{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, 
   OverloadedStrings #-}
{-# OPTIONS_GHC -Wall,
  -Wno-unused-imports #-}
import Data.Monoid


-- some comment
class Semigroup a => SomeData a

instance SomeData All
