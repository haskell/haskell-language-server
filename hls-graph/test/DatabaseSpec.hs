{-# LANGUAGE OverloadedStrings #-}

module DatabaseSpec where

import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Trans.Cont                (evalContT)
import           Development.IDE.Graph                   (newKey, shakeOptions)
import           Development.IDE.Graph.Database          (shakeNewDatabase,
                                                          shakeRunDatabase)
import           Development.IDE.Graph.Internal.Action   (apply1)
import           Development.IDE.Graph.Internal.Database (compute, incDatabase)
import           Development.IDE.Graph.Internal.Rules    (addRule)
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.WorkerThread
import           Example
import           System.Time.Extra                       (timeout)
import           Test.Hspec


itInThread :: String -> (TaskQueue (IO ()) -> IO ()) -> SpecWith ()
itInThread name ex = it name $ evalContT $ do
    thread <- withWorkerQueueSimple (const $ return ()) "hls-graph test"
    liftIO $ ex thread

spec :: Spec
spec = do
    describe "Evaluation" $ do
        itInThread "detects cycles" $ \q -> do
            db <- shakeNewDatabase q shakeOptions $ do
                ruleBool
                addRule $ \Rule _old _mode -> do
                    True <- apply1 (Rule @Bool)
                    return $ RunResult ChangedRecomputeDiff "" () (return ())
            let res = shakeRunDatabase db $ pure $ apply1 (Rule @())
            timeout 1 res `shouldThrow` \StackException{} -> True

    describe "compute" $ do
      itInThread "build step and changed step updated correctly" $ \q -> do
        (ShakeDatabase _ _ theDb) <- shakeNewDatabase q shakeOptions $ do
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
