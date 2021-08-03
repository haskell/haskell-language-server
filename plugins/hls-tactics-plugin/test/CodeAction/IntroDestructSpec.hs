{-# LANGUAGE OverloadedStrings #-}

module CodeAction.IntroDestructSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let test l c = goldenTest IntroAndDestruct "" l c
               . mappend "IntroDestruct"

  describe "golden" $ do
    test 4  5 "One"
    test 2  5 "Many"
    test 4 11 "LetBinding"

  describe "provider" $ do
    mkTest
      "Can intro and destruct an algebraic ty"
      "IntroDestructProvider" 2 12
      [ (id, IntroAndDestruct, "")
      ]
    mkTest
      "Won't intro and destruct a non-algebraic ty"
      "IntroDestructProvider" 5 12
      [ (not, IntroAndDestruct, "")
      ]
    mkTest
      "Can't intro, so no option"
      "IntroDestructProvider" 8 17
      [ (not, IntroAndDestruct, "")
      ]

