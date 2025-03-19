{-# LANGUAGE TemplateHaskell #-}
module THA where
import Language.Haskell.TH
import A (foo)

th_a :: DecsQ
th_a = [d| a = foo |]
