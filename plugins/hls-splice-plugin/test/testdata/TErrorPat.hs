{-# LANGUAGE TemplateHaskell #-}
module TErrorPat where
import Language.Haskell.TH ( conP )

f :: () -> ()
f $(conP 'True []) = x
