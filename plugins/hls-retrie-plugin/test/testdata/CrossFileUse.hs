module CrossFileUse where

import CrossFileDef (e)

-- the expression requires an import
f :: Int
f = e (Just 7)
