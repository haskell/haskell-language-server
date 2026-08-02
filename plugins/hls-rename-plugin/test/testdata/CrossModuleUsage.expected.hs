module CrossModuleUse (incrementUsage) where

import CrossModuleDefinition (succInt)

incrementUsage :: Int -> Int
incrementUsage x = succInt x
