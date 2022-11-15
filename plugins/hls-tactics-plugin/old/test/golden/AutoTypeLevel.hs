{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

import Data.Kind

data Nat = Z | S Nat

data HList (ls :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

data ElemAt (n :: Nat) t (ts :: [Type]) where
  AtZ :: ElemAt 'Z t (t ': ts)
  AtS :: ElemAt k t ts -> ElemAt ('S k) t (u ': ts)

lookMeUp :: ElemAt i ty tys -> HList tys -> ty
lookMeUp = _

