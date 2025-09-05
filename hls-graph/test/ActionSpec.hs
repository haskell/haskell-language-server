{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ActionSpec where

import           Control.Concurrent                      (MVar, readMVar)
import qualified Control.Concurrent                      as C
import           Control.Concurrent.STM
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Trans.Cont                (evalContT)
import           Development.IDE.Graph                   (shakeOptions)
import           Development.IDE.Graph.Database          (shakeNewDatabase,
                                                          shakeRunDatabase,
                                                          shakeRunDatabaseForKeys)
import           Development.IDE.Graph.Internal.Database (build, incDatabase)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Rule
import           Development.IDE.WorkerThread            (TaskQueue,
                                                          withWorkerQueueSimple)
import           Example
import qualified StmContainers.Map                       as STM
import           Test.Hspec



itInThread :: String -> (TaskQueue (IO ()) -> IO ()) -> SpecWith ()
itInThread name ex = it name $ evalContT $ do
    thread <- withWorkerQueueSimple (const $ return ()) "hls-graph test"
    liftIO $ ex thread

spec :: Spec
spec = do
  describe "apply1" $ itInThread "Test build update, Buggy dirty mechanism in hls-graph #4237" $ \q -> do
    let ruleStep1 :: MVar Int -> Rules ()
        ruleStep1 m = addRule $ \CountRule _old mode -> do
            -- depends on ruleSubBranch, it always changed if dirty
            _ :: Int <- apply1 SubBranchRule
            let r = 1
            case mode of
                -- it update the built step
                RunDependenciesChanged -> do
                    _ <- liftIO $ C.modifyMVar m $ \x -> return (x+1, x)
                    return $ RunResult ChangedRecomputeSame "" r (return ())
                -- this won't update the built step
                RunDependenciesSame ->
                    return $ RunResult ChangedNothing "" r (return ())
    count <- C.newMVar 0
    count1 <- C.newMVar 0
    db <- shakeNewDatabase q shakeOptions $ do
      ruleSubBranch count
      ruleStep1 count1
    -- bootstrapping the database
    _ <- shakeRunDatabase db $ pure $ apply1 CountRule -- count = 1
    let child = newKey SubBranchRule
    let parent = newKey CountRule
    -- instruct to RunDependenciesChanged then CountRule should be recomputed
    -- result should be changed 0, build 1
    _res1 <- shakeRunDatabaseForKeys (Just [child]) db [apply1 CountRule] -- count = 2
    -- since child changed = parent build
    -- instruct to RunDependenciesSame then CountRule should not be recomputed
    -- result should be changed 0, build 1
    _res3 <- shakeRunDatabaseForKeys (Just [parent]) db [apply1 CountRule] -- count = 2
    -- invariant child changed = parent build should remains after RunDependenciesSame
    -- this used to be a bug, with additional computation, see https://github.com/haskell/haskell-language-server/pull/4238
    _res3 <- shakeRunDatabaseForKeys (Just [parent]) db [apply1 CountRule] -- count = 2
    c1 <- readMVar count1
    c1 `shouldBe` 2
  describe "apply1" $  do
    itInThread "computes a rule with no dependencies" $ \q -> do
      db <- shakeNewDatabase q shakeOptions ruleUnit
      res <- shakeRunDatabase db $
        pure $ apply1 (Rule @())
      res `shouldBe` [()]
    itInThread "computes a rule with one dependency" $ \q -> do
      db <- shakeNewDatabase q shakeOptions $ do
        ruleUnit
        ruleBool
      res <- shakeRunDatabase db $ pure $ apply1 Rule
      res `shouldBe` [True]
    itInThread "tracks direct dependencies" $ \q -> do
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase q shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
      resultDeps res `shouldBe` ResultDeps [singletonKeySet $ newKey (Rule @())]
    itInThread "tracks reverse dependencies" $ \q -> do
      db@(ShakeDatabase _ _ Database {..}) <- shakeNewDatabase q shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just KeyDetails {..} <- atomically $ STM.lookup (newKey (Rule @())) databaseValues
      keyReverseDeps `shouldBe` singletonKeySet (newKey theKey)
    itInThread "rethrows exceptions" $ \q -> do
      db <- shakeNewDatabase q shakeOptions $ addRule $ \(Rule :: Rule ()) _old _mode -> error "boom"
      let res = shakeRunDatabase db $ pure $ apply1 (Rule @())
      res `shouldThrow` anyErrorCall
    itInThread "computes a rule with branching dependencies does not invoke phantom dependencies #3423" $ \q -> do
      cond <- C.newMVar True
      count <- C.newMVar 0
      (ShakeDatabase _ _ theDb) <- shakeNewDatabase q shakeOptions $ do
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

  describe "applyWithoutDependency" $ itInThread "does not track dependencies" $ \q -> do
    db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase q shakeOptions $ do
      ruleUnit
      addRule $ \Rule _old _mode -> do
          [()] <- applyWithoutDependency [Rule]
          return $ RunResult ChangedRecomputeDiff "" True $ return ()

    let theKey = Rule @Bool
    res <- shakeRunDatabase db $
      pure $ applyWithoutDependency [theKey]
    res `shouldBe` [[True]]
    Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
    resultDeps res `shouldBe` UnknownDeps
