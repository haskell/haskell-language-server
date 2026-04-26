{-# LANGUAGE TemplateHaskell #-}
module TDeclForeignImport where
import Language.Haskell.TH

foreign import ccall unsafe "math.h sin" c_sin :: Double -> Double
