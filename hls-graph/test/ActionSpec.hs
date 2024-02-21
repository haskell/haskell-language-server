{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ActionSpec where

import           Control.Concurrent.STM
import           Development.IDE.Graph                (shakeOptions)
import           Development.IDE.Graph.Database       (shakeNewDatabase,
                                                       shakeRunDatabase)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Rule
import           Example
import qualified StmContainers.Map                    as STM
import           Test.Hspec

spec :: Spec
spec = do
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
  describe "applyWithoutDependency" $ do
    it "does not track dependencies" $ do
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        addRule $ \Rule _old _mode -> do
            [()] <- applyWithoutDependency [Rule]
            return $ RunResult ChangedRecomputeDiff "" True

      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ do
          applyWithoutDependency [theKey]
      res `shouldBe` [[True]]
      Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
      resultDeps res `shouldBe` UnknownDeps
