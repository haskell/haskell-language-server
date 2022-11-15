{-# LANGUAGE TypeFamilies #-}
module AssociatedTypeFamily () where

class C a where
    type Fam a

x :: C a => a -> Fam a
x = undefined
