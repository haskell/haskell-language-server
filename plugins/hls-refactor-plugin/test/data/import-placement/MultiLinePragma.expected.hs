{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, 
   OverloadedStrings #-}
{-# OPTIONS_GHC -Wall,
import Data.Monoid
  -Wno-unused-imports #-}


-- some comment
class Semigroup a => SomeData a

instance SomeData All
