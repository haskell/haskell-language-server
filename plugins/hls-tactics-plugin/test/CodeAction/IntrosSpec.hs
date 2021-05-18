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

