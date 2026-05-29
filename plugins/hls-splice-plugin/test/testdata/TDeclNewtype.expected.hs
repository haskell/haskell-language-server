{-# LANGUAGE TemplateHaskell #-}
module TDeclNewtype where
import Language.Haskell.TH

newtype MyNewtype
  = MkMyNewtype Int
  deriving Show
