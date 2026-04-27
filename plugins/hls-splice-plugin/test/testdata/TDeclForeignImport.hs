{-# LANGUAGE TemplateHaskell #-}
module TDeclForeignImport where
import Language.Haskell.TH

$(pure <$> forImpD cCall unsafe "math.h sin" (mkName "c_sin") [t|Double -> Double|])
