{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.UseDataConSpec where

import qualified Data.Text as T
import           Ide.Plugin.Tactic.TestTypes
import           Test.Hspec
import           Utils


spec :: Spec
spec = do
  let useTest = goldenTest UseDataCon
  describe "provider" $ do
    mkTest
      "Suggests all data cons for Either"
      "ConProviders.hs" 5 6
      [ (id, UseDataCon, "Left")
      , (id, UseDataCon, "Right")
      , (not, UseDataCon, ":")
      , (not, UseDataCon, "[]")
      , (not, UseDataCon, "C1")
      ]
    mkTest
      "Suggests no data cons for big types"
      "ConProviders.hs" 11 17 $ do
        c <- [1 :: Int .. 10]
        pure $ (not, UseDataCon, T.pack $ show c)
    mkTest
      "Suggests only matching data cons for GADT"
      "ConProviders.hs" 20 12
      [ (id, UseDataCon, "IntGADT")
      , (id, UseDataCon, "VarGADT")
      , (not, UseDataCon, "BoolGADT")
      ]

  describe "golden" $ do
    useTest "(,)"   "UseConPair.hs"  2 8
    useTest "Left"  "UseConLeft.hs"  2 8
    useTest "Right" "UseConRight.hs" 2 8

