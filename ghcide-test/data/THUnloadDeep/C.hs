{-# LANGUAGE TemplateHaskell #-}
module C where

import A6
import Language.Haskell.TH

c :: Int
c = $(reportWarning ("a6 is " ++ show a6) >> [| a6 |])
