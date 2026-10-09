{-# LANGUAGE TemplateHaskell #-}
module THA where
import Language.Haskell.TH

th_a :: DecsQ
th_a = [d| a = () |]
