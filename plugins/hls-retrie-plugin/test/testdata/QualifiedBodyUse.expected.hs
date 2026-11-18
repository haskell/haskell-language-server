module QualifiedBodyUse where

import QualifiedBodyDef (e)
import qualified Data.Maybe as DM

f :: Int
f = DM.fromMaybe 0 (Just 7)
