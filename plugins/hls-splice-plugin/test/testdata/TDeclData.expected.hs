{-# LANGUAGE TemplateHaskell #-}
module TDeclData where
import Language.Haskell.TH

data MyData
  = MyConA Int | MyConB String
  deriving (Show, Eq)
