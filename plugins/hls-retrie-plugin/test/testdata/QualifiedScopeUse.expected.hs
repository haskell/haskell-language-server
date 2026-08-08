module QualifiedScopeUse where

import CrossFileDef (e)
import qualified Data.Maybe as M
import Data.Maybe (fromMaybe)

f :: Int
f = fromMaybe 0 (Just 7)
