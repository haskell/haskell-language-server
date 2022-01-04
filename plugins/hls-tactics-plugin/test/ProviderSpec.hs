{-# LANGUAGE OverloadedStrings #-}

module ProviderSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  mkTest
    "Produces intros code action"
    "T1" 2 14
    [ (id, Intros, "")
    ]

  mkTest
    "Won't suggest intros on the wrong type"
    "T2" 8 8
    [ (not, Intros, "")
    ]
