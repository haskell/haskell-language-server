{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.IntrosSpec where

import Ide.Plugin.Tactic.TestTypes
import Test.Hspec
import Utils


spec :: Spec
spec = do
  describe "golden" $ do
    goldenTest Intros "" "GoldenIntros.hs" 2 8

