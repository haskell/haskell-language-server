{-# LANGUAGE OverloadedStrings #-}

module CodeAction.IntrosSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let introsTest = goldenTest Intros ""

  describe "golden" $ do
    introsTest 2 8 "GoldenIntros"

  describe "layout" $ do
    introsTest 4 24 "LayoutRec"

