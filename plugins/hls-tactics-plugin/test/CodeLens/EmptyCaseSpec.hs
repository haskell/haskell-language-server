{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeLens.EmptyCaseSpec where

import Test.Hspec
import Utils
import Wingman.FeatureSet (allFeatures)


spec :: Spec
spec = do
  let test = mkCodeLensTest allFeatures

  describe "golden" $ do
    test "EmptyCaseADT.hs"
    test "EmptyCaseShadow.hs"
    test "EmptyCaseParens.hs"
    test "EmptyCaseNested.hs"
    test "EmptyCaseApply.hs"
    test "EmptyCaseGADT.hs"

