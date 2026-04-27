{-# LANGUAGE TemplateHaskell #-}
module TDeclData where
import Language.Haskell.TH

MyData
  = MyConA Int | MyConB String
  deriving (Show, Eq)
