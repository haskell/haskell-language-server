module FixityUse where

import FixityDef

foo :: Char -> Maybe Int -> Maybe String
foo c mInt = show <$> mInt <!> pure <$> Just c
