{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.DestructAllSpec where

import Ide.Plugin.Tactic.TestTypes
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
    destructAllTest "DestructAllAnd.hs"  2 11
    destructAllTest "DestructAllMany.hs" 4 23
    destructAllTest "DestructAllNonVarTopMatch.hs" 2 18

