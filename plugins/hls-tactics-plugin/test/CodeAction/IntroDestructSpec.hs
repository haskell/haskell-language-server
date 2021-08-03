{-# LANGUAGE OverloadedStrings #-}

module CodeAction.IntroDestructSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let test l c = goldenTest IntroAndDestruct "" l c
               . mappend "IntroDestruct"

  describe "golden" $ do
    test 4  5 "One"
    test 2  5 "Many"
    test 4 11 "LetBinding"

