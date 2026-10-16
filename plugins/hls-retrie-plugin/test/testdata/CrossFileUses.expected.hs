module CrossFileUses where

import CrossFileDef (e)
import Data.Maybe (fromMaybe)

-- only this use is inlined, so the import is added once
f :: Int
f = fromMaybe 0 (Just 7)

g :: Int
g = e Nothing
