{-# LANGUAGE OverloadedStrings #-}

module CodeAction.AutoSpec where

import Wingman.Types
import Test.Hspec
import Utils


spec :: Spec
spec = do
  let autoTest = goldenTest Auto ""
      autoTestNoWhitespace = goldenTestNoWhitespace Auto ""

  describe "golden" $ do
    autoTest 11  8 "AutoSplitGADT"
    autoTest  2 11 "GoldenEitherAuto"
    autoTest  4 12 "GoldenJoinCont"
    autoTest  3 11 "GoldenIdentityFunctor"
    autoTest  7 11 "GoldenIdTypeFam"
    autoTest  2 15 "GoldenEitherHomomorphic"
    autoTest  2  8 "GoldenNote"
    autoTest  2 12 "GoldenPureList"
    autoTest  2 12 "GoldenListFmap"
    autoTest  2 13 "GoldenFromMaybe"
    autoTest  2 10 "GoldenFoldr"
    autoTest  2  8 "GoldenSwap"
    autoTest  4 11 "GoldenFmapTree"
    autoTest  7 13 "GoldenGADTAuto"
    autoTest  2 12 "GoldenSwapMany"
    autoTest  4 12 "GoldenBigTuple"
    autoTest  2 10 "GoldenShow"
    autoTest  2 15 "GoldenShowCompose"
    autoTest  2  8 "GoldenShowMapChar"
    autoTest  7  8 "GoldenSuperclass"
    autoTest  2 12 "GoldenSafeHead"
    autoTest  2 12 "FmapBoth"
    autoTest  7  8 "RecordCon"
    autoTest  6  8 "NewtypeRecord"
    autoTest  2 14 "FmapJoin"
    autoTest  2  9 "Fgmap"
    autoTest  4 19 "FmapJoinInLet"
    autoTest  9 12 "AutoEndo"
    autoTest  2 16 "AutoEmptyString"
    autoTest  7 35 "AutoPatSynUse"
    autoTest  2 28 "AutoZip"
    autoTest  2 17 "AutoInfixApply"
    autoTest  2 19 "AutoInfixApplyMany"
    autoTest  2 25 "AutoInfixInfix"
    autoTest 19 12 "AutoTypeLevel"
    autoTest 11  9 "AutoForallClassMethod"
    autoTest  2  8 "AutoUnusedPatternMatch"

    failing "flaky in CI" $
      autoTest 2 11 "GoldenApplicativeThen"

    failing "not enough auto gas" $
      autoTest 5 18 "GoldenFish"

  describe "theta" $ do
    autoTest 12 10 "AutoThetaFix"
    autoTest  7 27 "AutoThetaRankN"
    autoTest  6 10 "AutoThetaGADT"
    autoTest  6  8 "AutoThetaGADTDestruct"
    autoTest  4  8 "AutoThetaEqCtx"
    autoTest  6 10 "AutoThetaEqGADT"
    autoTest  6  8 "AutoThetaEqGADTDestruct"
    autoTest  6 10 "AutoThetaRefl"
    autoTest  6  8 "AutoThetaReflDestruct"
    autoTest 19 30 "AutoThetaMultipleUnification"
    autoTest 16  9 "AutoThetaSplitUnification"

  describe "known" $ do
    autoTest 25 13 "GoldenArbitrary"
    autoTest  6 13 "GoldenArbitrarySingleConstructor"
    autoTestNoWhitespace
              6 10 "KnownBigSemigroup"
    autoTest  4 10 "KnownThetaSemigroup"
    autoTest  6 10 "KnownCounterfactualSemigroup"
    autoTest 10 10 "KnownModuleInstanceSemigroup"
    autoTest  4 22 "KnownDestructedSemigroup"
    autoTest  4 10 "KnownMissingSemigroup"
    autoTest  7 12 "KnownMonoid"
    autoTest  7 12 "KnownPolyMonoid"
    autoTest  7 12 "KnownMissingMonoid"


  describe "messages" $ do
    mkShowMessageTest Auto ""  2 8 "MessageForallA"      TacticErrors
    mkShowMessageTest Auto ""  7 8 "MessageCantUnify"    TacticErrors
    mkShowMessageTest Auto "" 12 8 "MessageNotEnoughGas" NotEnoughGas

