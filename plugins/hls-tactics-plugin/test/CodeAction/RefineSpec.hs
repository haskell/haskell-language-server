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
    refineTest "RefineIntro.hs"  2 8
    refineTest "RefineCon.hs"    2 8
    refineTest "RefineReader.hs" 4 8
    refineTest "RefineGADT.hs"   8 8

