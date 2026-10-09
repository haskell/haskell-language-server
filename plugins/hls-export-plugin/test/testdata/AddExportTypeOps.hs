{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module AddExportTypeOps (placeholder) where

placeholder :: Int
placeholder = 0

type (:<>) = ()

type family (:+:)

class (:*:) a

newtype (:->) = MkArr ()

pattern x :++ y = (x, y)
