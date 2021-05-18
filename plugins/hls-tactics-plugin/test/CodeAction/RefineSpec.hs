{-# LANGUAGE OverloadedStrings #-}

module CodeAction.RefineSpec where

import Wingman.Types
import Test.Hspec
import Utils
import Wingman.FeatureSet (allFeatures)


spec :: Spec
spec = do
  let refineTest = goldenTest Refine ""

  describe "golden" $ do
    refineTest  2  8 "RefineIntro"
    refineTest  2  8 "RefineCon"
    refineTest  4 10 "RefineReader"
    refineTest  8 10 "RefineGADT"

  describe "messages" $ do
    mkShowMessageTest allFeatures Refine "" 2 8 "MessageForallA" TacticErrors

