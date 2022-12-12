{-# LANGUAGE OverloadedStrings #-}

module CodeAction.DestructSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let destructTest = goldenTest Destruct

  describe "golden" $ do
    destructTest "gadt"  7 17 "GoldenGADTDestruct"
    destructTest "gadt"  8 17 "GoldenGADTDestructCoercion"
    destructTest "a"     7 25 "SplitPattern"
    destructTest "a"     6 18 "DestructPun"
    destructTest "fp"   31 14 "DestructCthulhu"
    destructTest "b"     7 10 "DestructTyFam"
    destructTest "b"     7 10 "DestructDataFam"
    destructTest "b"    17 10 "DestructTyToDataFam"
    destructTest "t"     6 10 "DestructInt"

  describe "layout" $ do
    destructTest "b"  4  3 "LayoutBind"
    destructTest "b"  2 15 "LayoutDollarApp"
    destructTest "b"  2 18 "LayoutOpApp"
    destructTest "b"  2 14 "LayoutLam"
    destructTest "x" 11 15 "LayoutSplitWhere"
    destructTest "x"  3 12 "LayoutSplitClass"
    destructTest "b"  3  9 "LayoutSplitGuard"
    destructTest "b"  4 13 "LayoutSplitLet"
    destructTest "a"  4  7 "LayoutSplitIn"
    destructTest "a"  4 31 "LayoutSplitViewPat"
    destructTest "a"  7 17 "LayoutSplitPattern"
    destructTest "a"  8 26 "LayoutSplitPatSyn"

  describe "providers" $ do
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

    mkTest
      "Suggests homomorphism if the domain is bigger than the codomain"
      "ProviderHomomorphism" 12 13
      [ (id, Homomorphism, "g")
      ]

    mkTest
      "Doesn't suggest homomorphism if the domain is smaller than the codomain"
      "ProviderHomomorphism" 15 14
      [ (not, Homomorphism, "g")
      , (id, Destruct, "g")
      ]

    mkTest
      "Suggests lambda homomorphism if the domain is bigger than the codomain"
      "ProviderHomomorphism" 18 14
      [ (id, HomomorphismLambdaCase, "")
      ]

    mkTest
      "Doesn't suggest lambda homomorphism if the domain is smaller than the codomain"
      "ProviderHomomorphism" 21 15
      [ (not, HomomorphismLambdaCase, "")
      , (id, DestructLambdaCase, "")
      ]

    -- test layouts that maintain user-written fixities
    destructTest "b"  3 13 "LayoutInfixKeep"
    destructTest "b"  2 12 "LayoutPrefixKeep"

