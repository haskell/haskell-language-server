{-# LANGUAGE TemplateHaskell #-}
module TDeclInstance where
import Language.Haskell.TH

data Wrapper = MkWrapper Int
instance Show Wrapper where
  show (MkWrapper n) = ("Wrapper:" ++ show n)
