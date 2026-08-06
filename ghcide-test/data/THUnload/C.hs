{-# LANGUAGE TemplateHaskell #-}
module C where

import B
import Language.Haskell.TH

c :: Int
c = $(reportWarning ("b is " ++ show b) >> [| b |])
