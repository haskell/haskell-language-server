{-# LANGUAGE TemplateHaskell #-}
module TDeclForeignImport where
import Language.Haskell.TH

 importccall unsafe "math.h sin" c_sin :: Double -> Double
