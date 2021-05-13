{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.RunMetaprogramSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let metaTest = goldenTest RunMetaprogram ""

  describe "beginMetaprogram" $ do
    goldenTest BeginMetaprogram ""  1  7 "MetaBegin.hs"

  describe "golden" $ do
    metaTest  6 11 "MetaMaybeAp.hs"
    metaTest  2 32 "MetaBindOne.hs"
    metaTest  2 32 "MetaBindAll.hs"
    metaTest  2 13 "MetaTry.hs"
    metaTest  2 74 "MetaChoice.hs"

