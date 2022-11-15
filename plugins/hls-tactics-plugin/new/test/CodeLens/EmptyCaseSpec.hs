{-# LANGUAGE OverloadedStrings #-}

module CodeLens.EmptyCaseSpec where

import Test.Hspec
import Utils


spec :: Spec
spec = do
  let test = mkCodeLensTest
      noTest = mkNoCodeLensTest

  describe "code_lenses" $ do
    test "EmptyCaseADT"
    test "EmptyCaseShadow"
    test "EmptyCaseParens"
    test "EmptyCaseNested"
    test "EmptyCaseApply"
    test "EmptyCaseGADT"
    test "EmptyCaseLamCase"

  describe "no code lenses" $ do
    noTest "EmptyCaseSpuriousGADT"

