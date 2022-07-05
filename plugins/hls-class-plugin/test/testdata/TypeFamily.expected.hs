{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module TypeFamily where

class F a where
  type Elem a
  f :: Elem a -> a

instance Eq a => F [a] where
  f :: Eq a => Elem [a] -> [a]
  f = _
