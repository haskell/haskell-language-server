module ChildExportUse where

import ChildExportDef (e)
import ChildExportDef (method)

f :: Int
f = method (7 :: Int)
