{-# LANGUAGE OverloadedStrings #-}

module CodeLens.EmptyCaseSpec where

import Test.Hspec
import Utils


spec :: Spec
spec = do
  let test = mkCodeLensTest

  describe "golden" $ do
    test "EmptyCaseADT"
    test "EmptyCaseShadow"
    test "EmptyCaseParens"
    test "EmptyCaseNested"
    test "EmptyCaseApply"
    test "EmptyCaseGADT"

