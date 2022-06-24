{-# LANGUAGE InstanceSigs #-}
module Qualified where
import qualified QualifiedA

class F a where
    f :: a

instance F QualifiedA.A where
  f :: QualifiedA.A
  f = undefined
