{-# LANGUAGE TemplateHaskell #-}
module TSimplePat where
import Language.Haskell.TH ( varP, mkName )

f :: x -> x
f $(varP $ mkName "x") = x
