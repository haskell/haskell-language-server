module CrossFileUse where

import CrossFileDef (e)
import Data.Maybe (fromMaybe)

-- the expression requires an import
f :: Int
f = fromMaybe 0 (Just 7)
