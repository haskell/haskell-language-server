{-# LANGUAGE OverloadedStrings #-}

module CodeAction.DestructPunSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let destructTest = goldenTest DestructPun

  describe "golden" $ do
    destructTest "x"  4  9 "PunSimple"
    destructTest "x"  6 10 "PunMany"
    destructTest "x" 11 11 "PunGADT"
    destructTest "x" 17 11 "PunManyGADT"
    destructTest "x"  4 12 "PunShadowing"

