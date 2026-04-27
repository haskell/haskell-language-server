{-# LANGUAGE TemplateHaskell #-}
module TDeclNewtype where
import Language.Haskell.TH

MyNewtype
  = MkMyNewtype Int
  deriving Show
