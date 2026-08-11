module CrossModuleUse (incrementUsage) where

import CrossModuleDefinition (succInt)

incrementUsage :: Int -> Int
incrementUsage x = succInt x

incrementUsage2 :: Int -> Int
incrementUsage2 = succInt . succInt

incrementUsage3 :: Int -> Int
incrementUsage3 x = succInt (succInt (succInt x))
