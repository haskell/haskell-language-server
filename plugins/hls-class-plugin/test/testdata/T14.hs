{-# LANGUAGE GHC2021 #-}
module T14 where

data A
instance Eq A where
    (==) = _
