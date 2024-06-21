{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where

import           Development.IDE.Graph                   (newKey, shakeOptions)
import           Development.IDE.Graph.Database          (shakeNewDatabase,
                                                          shakeRunDatabase)
import           Development.IDE.Graph.Internal.Action   (apply1)
import           Development.IDE.Graph.Internal.Database (compute, incDatabase)
import           Development.IDE.Graph.Internal.Rules    (addRule)
import           Development.IDE.Graph.Internal.Types
import           Example
import           System.Time.Extra                       (timeout)
import           Test.Hspec


spec :: Spec
spec = do
    describe "Evaluation" $ do
        it "detects cycles" $ do
            db <- shakeNewDatabase shakeOptions $ do
                ruleBool
                addRule $ \Rule _old _mode -> do
                    True <- apply1 (Rule @Bool)
                    return $ RunResult ChangedRecomputeDiff "" () (return ())
            let res = shakeRunDatabase db $ pure $ apply1 (Rule @())
            timeout 1 res `shouldThrow` \StackException{} -> True

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
