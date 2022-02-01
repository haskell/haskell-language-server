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

  goldenTestMany "SubsequentTactics"
    [ InvokeTactic Intros   ""   4  5
    , InvokeTactic Destruct "du" 4  8
    , InvokeTactic Auto     ""   4 15
    ]
