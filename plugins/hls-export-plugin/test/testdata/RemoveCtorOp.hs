{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}
module RemoveCtorOp (type (:+:) (Op)) where

data a :+: b = Op
