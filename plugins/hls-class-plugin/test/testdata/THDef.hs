{-# LANGUAGE TemplateHaskell #-}

module THDef where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class F a where
    f :: a

gen :: Lift t => Name -> t -> Q [Dec]
gen ty v = [d| instance F $(conT ty) where f = v |]
