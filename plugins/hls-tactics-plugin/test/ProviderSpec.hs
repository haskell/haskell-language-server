{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module ProviderSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  mkTest
    "Produces intros code action"
    "T1.hs" 2 14
    [ (id, Intros, "")
    ]
  mkTest
    "Produces destruct and homomorphism code actions"
    "T2.hs" 2 21
    [ (id, Destruct, "eab")
    , (id, Homomorphism, "eab")
    ]
  mkTest
    "Won't suggest homomorphism on the wrong type"
    "T2.hs" 8 8
    [ (not, Homomorphism, "global")
    ]
  mkTest
    "Won't suggest intros on the wrong type"
    "T2.hs" 8 8
    [ (not, Intros, "")
    ]
  mkTest
    "Produces (homomorphic) lambdacase code actions"
    "T3.hs" 4 24
    [ (id, HomomorphismLambdaCase, "")
    , (id, DestructLambdaCase, "")
    ]
  mkTest
    "Produces lambdacase code actions"
    "T3.hs" 7 13
    [ (id, DestructLambdaCase, "")
    ]
  mkTest
    "Doesn't suggest lambdacase without -XLambdaCase"
    "T2.hs" 11 25
    [ (not, DestructLambdaCase, "")
    ]

