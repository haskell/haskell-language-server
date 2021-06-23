{-# LANGUAGE OverloadedStrings #-}

module CodeAction.DestructSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let destructTest = goldenTest Destruct

  describe "golden" $ do
    destructTest "gadt"  7 17 "GoldenGADTDestruct"
    destructTest "gadt"  8 17 "GoldenGADTDestructCoercion"
    destructTest "a"     7 25 "SplitPattern"
    destructTest "a"     6 18 "DestructPun"
    destructTest "fp"   31 14 "DestructCthulhu"
    destructTest "b"     7 10 "DestructTyFam"
    destructTest "b"     7 10 "DestructDataFam"
    destructTest "b"    17 10 "DestructTyToDataFam"

  describe "layout" $ do
    destructTest "b"  4  3 "LayoutBind"
    destructTest "b"  2 15 "LayoutDollarApp"
    destructTest "b"  2 18 "LayoutOpApp"
    destructTest "b"  2 14 "LayoutLam"
    destructTest "x" 11 15 "LayoutSplitWhere"
    destructTest "x"  3 12 "LayoutSplitClass"
    destructTest "b"  3  9 "LayoutSplitGuard"
    destructTest "b"  4 13 "LayoutSplitLet"
    destructTest "a"  4  7 "LayoutSplitIn"
    destructTest "a"  4 31 "LayoutSplitViewPat"
    destructTest "a"  7 17 "LayoutSplitPattern"
    destructTest "a"  8 26 "LayoutSplitPatSyn"

