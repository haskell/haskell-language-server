{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module CodeAction.AutoSpec where

import Wingman.Types
import Test.Hspec
import Utils
import Wingman.FeatureSet (allFeatures)


spec :: Spec
spec = do
  let autoTest = goldenTest Auto ""
      autoTestNoWhitespace = goldenTestNoWhitespace Auto ""

  describe "golden" $ do
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
    autoTest  2 12 "FmapBoth.hs"
    autoTest  7  8 "RecordCon.hs"
    autoTest  6  8 "NewtypeRecord.hs"
    autoTest  2 14 "FmapJoin.hs"
    autoTest  2  9 "Fgmap.hs"
    autoTest  4 19 "FmapJoinInLet.hs"
    autoTest  9 12 "AutoEndo.hs"
    autoTest  2 16 "AutoEmptyString.hs"
    autoTest  7 35 "AutoPatSynUse.hs"
    autoTest  2 28 "AutoZip.hs"

    failing "flaky in CI" $
      autoTest 2 11 "GoldenApplicativeThen.hs"

    failing "not enough auto gas" $
      autoTest 5 18 "GoldenFish.hs"

  describe "theta" $ do
    autoTest 12 10 "AutoThetaFix.hs"
    autoTest  7 20 "AutoThetaRankN.hs"
    autoTest  6 10 "AutoThetaGADT.hs"
    autoTest  6  8 "AutoThetaGADTDestruct.hs"
    autoTest  4  8 "AutoThetaEqCtx.hs"
    autoTest  6 10 "AutoThetaEqGADT.hs"
    autoTest  6  8 "AutoThetaEqGADTDestruct.hs"
    autoTest  6 10 "AutoThetaRefl.hs"
    autoTest  6  8 "AutoThetaReflDestruct.hs"

  describe "known" $ do
    autoTest 25 13 "GoldenArbitrary.hs"
    autoTestNoWhitespace
              6 10 "KnownBigSemigroup.hs"
    autoTest  4 10 "KnownThetaSemigroup.hs"
    autoTest  6 10 "KnownCounterfactualSemigroup.hs"
    autoTest 10 10 "KnownModuleInstanceSemigroup.hs"
    autoTest  4 22 "KnownDestructedSemigroup.hs"
    autoTest  4 10 "KnownMissingSemigroup.hs"
    autoTest  7 12 "KnownMonoid.hs"
    autoTest  7 12 "KnownPolyMonoid.hs"
    autoTest  7 12 "KnownMissingMonoid.hs"


  describe "messages" $ do
    mkShowMessageTest allFeatures Auto "" 2 8 "MessageForallA.hs" TacticErrors

