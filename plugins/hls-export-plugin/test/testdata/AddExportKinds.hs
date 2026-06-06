{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module AddExportKinds (placeholder) where

placeholder :: Int
placeholder = 0

(<|) :: a -> a -> a
(<|) x _ = x

a `f` b = b

newtype NT = NT ()

type Syn = ()

type family TF p

pattern Pat :: a -> (a, a)
pattern Pat a = (a, a)

data (:<) = Mk
