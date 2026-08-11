module CrossModuleUse (incrementUsage) where

import CrossModuleDefinition (increment)

incrementUsage :: Int -> Int
incrementUsage x = increment x

incrementUsage2 :: Int -> Int
incrementUsage2 = increment . increment

incrementUsage3 :: Int -> Int
incrementUsage3 x = increment (increment (increment x))
