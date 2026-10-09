{-# LANGUAGE TemplateHaskell #-}

module Clash.Promoted.Nat.Literals where

import Clash.Promoted.Nat.TH

$(snatSplice)
