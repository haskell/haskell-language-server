{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.DestructPunSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let destructTest = goldenTest DestructPun

  describe "golden" $ do
    destructTest "x"  4  9 "PunSimple.hs"
    destructTest "x"  6 10 "PunMany.hs"
    destructTest "x" 11 11 "PunGADT.hs"
    destructTest "x" 17 11 "PunManyGADT.hs"
    destructTest "x"  4 12 "PunShadowing.hs"

