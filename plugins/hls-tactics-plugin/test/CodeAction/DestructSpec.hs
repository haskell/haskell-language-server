{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.DestructSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let destructTest = goldenTest Destruct

  describe "golden" $ do
    destructTest "gadt" 7 17 "GoldenGADTDestruct.hs"
    destructTest "gadt" 8 17 "GoldenGADTDestructCoercion.hs"
    destructTest "a"    7 25 "SplitPattern.hs"
    destructTest "a"    6 18 "DestructPun.hs"

  describe "layout" $ do
    destructTest "b"  4  3 "LayoutBind.hs"
    destructTest "b"  2 15 "LayoutDollarApp.hs"
    destructTest "b"  2 18 "LayoutOpApp.hs"
    destructTest "b"  2 14 "LayoutLam.hs"
    destructTest "x" 11 15 "LayoutSplitWhere.hs"
    destructTest "x"  3 12 "LayoutSplitClass.hs"
    destructTest "b"  3  9 "LayoutSplitGuard.hs"
    destructTest "b"  4 13 "LayoutSplitLet.hs"
    destructTest "a"  4  7 "LayoutSplitIn.hs"
    destructTest "a"  4 31 "LayoutSplitViewPat.hs"
    destructTest "a"  7 17 "LayoutSplitPattern.hs"
    destructTest "a"  8 26 "LayoutSplitPatSyn.hs"

