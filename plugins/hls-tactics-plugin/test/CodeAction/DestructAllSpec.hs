{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.DestructAllSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let destructAllTest = goldenTest DestructAll ""
  describe "provider" $ do
    mkTest
      "Requires args on lhs of ="
      "DestructAllProvider.hs" 3 21
      [ (not, DestructAll, "")
      ]
    mkTest
      "Can't be a non-top-hole"
      "DestructAllProvider.hs" 8 19
      [ (not, DestructAll, "")
      , (id, Destruct, "a")
      , (id, Destruct, "b")
      ]
    mkTest
      "Provides a destruct all otherwise"
      "DestructAllProvider.hs" 12 22
      [ (id, DestructAll, "")
      ]

  describe "golden" $ do
    destructAllTest 2 11 "DestructAllAnd.hs"
    destructAllTest 4 23 "DestructAllMany.hs"
    destructAllTest 2 18 "DestructAllNonVarTopMatch.hs"
    destructAllTest 2 18 "DestructAllFunc.hs"

