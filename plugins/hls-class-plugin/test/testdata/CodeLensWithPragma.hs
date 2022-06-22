{-# LANGUAGE InstanceSigs #-}
module CodeLensWithPragma where

data A
instance Eq A where
    (==) = _
