{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ActionSpec where

import qualified Control.Concurrent                      as C
import           Control.Concurrent.STM
import           Development.IDE.Graph                   (shakeOptions)
import           Development.IDE.Graph.Database          (shakeNewDatabase,
                                                          shakeRunDatabase)
import           Development.IDE.Graph.Internal.Database (build, compute,
                                                          incDatabase)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Rule
import           Example
import qualified StmContainers.Map                       as STM
import           Test.Hspec

spec :: Spec
spec = do
  describe "compute" $ do
    it "build step and changed step updated correctly" $ do
      (ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleStep

      let k = newKey $ Rule @()
      -- ChangedRecomputeSame
      r1@Result{resultChanged=rc1, resultBuilt=rb1} <- compute theDb emptyStack k RunDependenciesChanged Nothing
      incDatabase theDb Nothing
      -- ChangedRecomputeSame
      r2@Result{resultChanged=rc2, resultBuilt=rb2} <- compute theDb emptyStack k RunDependenciesChanged (Just r1)
      incDatabase theDb Nothing
      -- changed Nothing
      Result{resultChanged=rc3, resultBuilt=rb3} <- compute theDb emptyStack k RunDependenciesSame (Just r2)
      rc1 `shouldBe` Step 0
      rc2 `shouldBe` Step 0
      rc3 `shouldBe` Step 0

      rb1 `shouldBe` Step 0
      rb2 `shouldBe` Step 1
      rb3 `shouldBe` Step 1

  describe "apply1" $ do
    it "computes a rule with no dependencies" $ do
      db <- shakeNewDatabase shakeOptions $ do
        ruleUnit
      res <- shakeRunDatabase db $
        pure $ do
          apply1 (Rule @())
      res `shouldBe` [()]
    it "computes a rule with one dependency" $ do
      db <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      res <- shakeRunDatabase db $ pure $ apply1 Rule
      res `shouldBe` [True]
    it "tracks direct dependencies" $ do
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ do
          apply1 theKey
      res `shouldBe` [True]
      Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
      resultDeps res `shouldBe` ResultDeps [singletonKeySet $ newKey (Rule @())]
    it "tracks reverse dependencies" $ do
      db@(ShakeDatabase _ _ Database {..}) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ do
          apply1 theKey
      res `shouldBe` [True]
      Just KeyDetails {..} <- atomically $ STM.lookup (newKey (Rule @())) databaseValues
      keyReverseDeps `shouldBe` (singletonKeySet $ newKey theKey)
    it "rethrows exceptions" $ do
      db <- shakeNewDatabase shakeOptions $ do
        addRule $ \(Rule :: Rule ()) _old _mode -> error "boom"
      let res = shakeRunDatabase db $ pure $ apply1 (Rule @())
      res `shouldThrow` anyErrorCall
    it "computes a rule with branching dependencies does not invoke phantom dependencies #3423" $ do
      cond <- C.newMVar True
      count <- C.newMVar 0
      (ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleCond cond
        ruleSubBranch count
        ruleWithCond
      -- build the one with the condition True
      -- This should call the SubBranchRule once
      -- cond rule would return different results each time
      res0 <- build theDb emptyStack [BranchedRule]
      snd res0 `shouldBe` [1 :: Int]
      incDatabase theDb Nothing
      -- build the one with the condition False
      -- This should not call the SubBranchRule
      res1 <- build theDb emptyStack [BranchedRule]
      snd res1 `shouldBe` [2 :: Int]
     -- SubBranchRule should be recomputed once before this (when the condition was True)
      countRes <- build theDb emptyStack [SubBranchRule]
      snd countRes `shouldBe` [1 :: Int]

  describe "applyWithoutDependency" $ do
    it "does not track dependencies" $ do
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        addRule $ \Rule _old _mode -> do
            [()] <- applyWithoutDependency [Rule]
            return $ RunResult ChangedRecomputeDiff "" True $ return ()

      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ do
          applyWithoutDependency [theKey]
      res `shouldBe` [[True]]
      Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
      resultDeps res `shouldBe` UnknownDeps
