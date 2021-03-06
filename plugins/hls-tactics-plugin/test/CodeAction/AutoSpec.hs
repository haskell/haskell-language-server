{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.AutoSpec where

import Ide.Plugin.Tactic.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let autoTest = goldenTest Auto ""

  describe "golden tests" $ do
    autoTest 11  8 "AutoSplitGADT.hs"
    autoTest  2 11 "GoldenEitherAuto.hs"
    autoTest  4 12 "GoldenJoinCont.hs"
    autoTest  3 11 "GoldenIdentityFunctor.hs"
    autoTest  7 11 "GoldenIdTypeFam.hs"
    autoTest  2 15 "GoldenEitherHomomorphic.hs"
    autoTest  2  8 "GoldenNote.hs"
    autoTest  2 12 "GoldenPureList.hs"
    autoTest  2 12 "GoldenListFmap.hs"
    autoTest  2 13 "GoldenFromMaybe.hs"
    autoTest  2 10 "GoldenFoldr.hs"
    autoTest  2  8 "GoldenSwap.hs"
    autoTest  4 11 "GoldenFmapTree.hs"
    autoTest  7 13 "GoldenGADTAuto.hs"
    autoTest  2 12 "GoldenSwapMany.hs"
    autoTest  4 12 "GoldenBigTuple.hs"
    autoTest  2 10 "GoldenShow.hs"
    autoTest  2 15 "GoldenShowCompose.hs"
    autoTest  2  8 "GoldenShowMapChar.hs"
    autoTest  7  8 "GoldenSuperclass.hs"
    autoTest  2 12 "GoldenSafeHead.hs"
    autoTest 25 13 "GoldenArbitrary.hs"
    autoTest  2 12 "FmapBoth.hs"
    autoTest  7  8 "RecordCon.hs"
    autoTest  6  8 "NewtypeRecord.hs"
    autoTest  2 14 "FmapJoin.hs"
    autoTest  2  9 "Fgmap.hs"
    autoTest  4 19 "FmapJoinInLet.hs"
    autoTest  9 12 "AutoEndo.hs"

    failing "flaky in CI" $
      autoTest 2 11 "GoldenApplicativeThen.hs"

    failing "not enough auto gas" $
      autoTest 5 18 "GoldenFish.hs"

