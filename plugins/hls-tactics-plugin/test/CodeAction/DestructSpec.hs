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

  describe "layout" $ do
    destructTest "b" 4  3 "LayoutBind.hs"
    destructTest "b" 2 15 "LayoutDollarApp.hs"
    destructTest "b" 2 18 "LayoutOpApp.hs"
    destructTest "b" 2 14 "LayoutLam.hs"

