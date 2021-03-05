{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.RefineSpec where

import Ide.Plugin.Tactic.TestTypes
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let refineTest = goldenTest Refine ""

  describe "golden" $ do
    refineTest 2 8 "RefineIntro.hs"
    refineTest 2 8 "RefineCon.hs"
    refineTest 4 8 "RefineReader.hs"
    refineTest 8 8 "RefineGADT.hs"

