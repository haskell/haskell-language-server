module QualifiedScopeUse where

import CrossFileDef (e)
import qualified Data.Maybe as M

f :: Int
f = e (Just 7)
