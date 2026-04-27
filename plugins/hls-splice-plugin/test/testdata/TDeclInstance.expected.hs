{-# LANGUAGE TemplateHaskell #-}
module TDeclInstance where
import Language.Haskell.TH

Wrapper = MkWrapper Int
Show Wrapper where
  show (MkWrapper n) = ("Wrapper:" ++ show n)
