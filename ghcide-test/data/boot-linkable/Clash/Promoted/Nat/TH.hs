{-# LANGUAGE TemplateHaskell #-}
module Clash.Promoted.Nat.TH (snatSplice) where

import Language.Haskell.TH (Q, Dec)
import Clash.Promoted.Nat ()

snatSplice :: Q [Dec]
snatSplice = pure []
