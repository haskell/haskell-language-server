{-# LANGUAGE OverloadedStrings #-}

module CodeAction.RefineSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let refineTest = goldenTest Refine ""

  describe "golden" $ do
    refineTest  2  8 "RefineIntro"
    refineTest  2  8 "RefineCon"
    refineTest  4 10 "RefineReader"
    refineTest  8 10 "RefineGADT"
    refineTest  2  8 "RefineIntroWhere"

  describe "messages" $ do
    mkShowMessageTest Refine "" 2 8 "MessageForallA" TacticErrors

