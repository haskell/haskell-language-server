module CrossModuleUse (incrementUsage) where

import CrossModuleDefinition (increment)

incrementUsage :: Int -> Int
incrementUsage x = increment x
