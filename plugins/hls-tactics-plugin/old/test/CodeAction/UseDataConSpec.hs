{-# LANGUAGE OverloadedStrings #-}

module CodeAction.UseDataConSpec where

import qualified Data.Text as T
import           Wingman.Types
import           Test.Hspec
import           Utils


spec :: Spec
spec = do
  let useTest = goldenTest UseDataCon

  describe "provider" $ do
    mkTest
      "Suggests all data cons for Either"
      "ConProviders" 5 6
      [ (id, UseDataCon, "Left")
      , (id, UseDataCon, "Right")
      , (not, UseDataCon, ":")
      , (not, UseDataCon, "[]")
      , (not, UseDataCon, "C1")
      ]
    mkTest
      "Suggests no data cons for big types"
      "ConProviders" 11 17 $ do
        c <- [1 :: Int .. 10]
        pure $ (not, UseDataCon, T.pack $ show c)
    mkTest
      "Suggests only matching data cons for GADT"
      "ConProviders" 20 12
      [ (id, UseDataCon, "IntGADT")
      , (id, UseDataCon, "VarGADT")
      , (not, UseDataCon, "BoolGADT")
      ]

  describe "golden" $ do
    useTest "(,)"   2 8 "UseConPair"
    useTest "Left"  2 8 "UseConLeft"
    useTest "Right" 2 8 "UseConRight"

