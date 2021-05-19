{-# LANGUAGE OverloadedStrings #-}

module CodeLens.EmptyCaseSpec where

import Test.Hspec
import Utils
import Wingman.FeatureSet (allFeatures)


spec :: Spec
spec = do
  let test = mkCodeLensTest allFeatures

  describe "golden" $ do
    test "EmptyCaseADT"
    test "EmptyCaseShadow"
    test "EmptyCaseParens"
    test "EmptyCaseNested"
    test "EmptyCaseApply"
    test "EmptyCaseGADT"

