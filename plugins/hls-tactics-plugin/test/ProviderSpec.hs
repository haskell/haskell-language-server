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
    "Produces destruct and homomorphism code actions"
    "T2" 2 21
    [ (id, Destruct, "eab")
    , (id, Homomorphism, "eab")
    , (not, DestructPun, "eab")
    ]
  mkTest
    "Won't suggest homomorphism on the wrong type"
    "T2" 8 8
    [ (not, Homomorphism, "global")
    ]
  mkTest
    "Won't suggest intros on the wrong type"
    "T2" 8 8
    [ (not, Intros, "")
    ]
  mkTest
    "Produces (homomorphic) lambdacase code actions"
    "T3" 4 24
    [ (id, HomomorphismLambdaCase, "")
    , (id, DestructLambdaCase, "")
    ]
  mkTest
    "Produces lambdacase code actions"
    "T3" 7 13
    [ (id, DestructLambdaCase, "")
    ]
  mkTest
    "Doesn't suggest lambdacase without -XLambdaCase"
    "T2" 11 25
    [ (not, DestructLambdaCase, "")
    ]

  mkTest
    "Doesn't suggest destruct if already destructed"
    "ProvideAlreadyDestructed" 6 18
    [ (not, Destruct, "x")
    ]

  mkTest
    "...but does suggest destruct if destructed in a different branch"
    "ProvideAlreadyDestructed" 9 7
    [ (id, Destruct, "x")
    ]

  mkTest
    "Doesn't suggest destruct on class methods"
    "ProvideLocalHyOnly" 2 12
    [ (not, Destruct, "mempty")
    ]

