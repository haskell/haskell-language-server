{-# LANGUAGE GHC2021 #-}
module CodeLensWithGHC2021 where

data A
instance Eq A where
    (==) :: A -> A -> Bool
    (==) = _
