module CrossFileUses where

import CrossFileDef (e)

-- only this use is inlined, so the import is added once
f :: Int
f = e (Just 7)

g :: Int
g = e Nothing
