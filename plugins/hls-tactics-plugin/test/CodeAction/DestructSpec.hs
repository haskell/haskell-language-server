{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.DestructSpec where

import Ide.Plugin.Tactic.TestTypes
import Test.Hspec
import Utils


spec :: Spec
spec = do
  describe "golden" $ do
    goldenTest Destruct "gadt"
             "GoldenGADTDestruct.hs"      7 17
    goldenTest Destruct "gadt"
             "GoldenGADTDestructCoercion.hs" 8 17
    goldenTest Destruct "a"
      "SplitPattern.hs"  7 25

  describe "layout" $ do
    goldenTest Destruct "b" "LayoutBind.hs" 4 3
    goldenTest Destruct "b" "LayoutDollarApp.hs" 2 15
    goldenTest Destruct "b" "LayoutOpApp.hs" 2 18
    goldenTest Destruct "b" "LayoutLam.hs" 2 14

