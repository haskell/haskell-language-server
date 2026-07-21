{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ActionSpec where

import           Control.Concurrent                      (MVar, readMVar)
import qualified Control.Concurrent                      as C
import           Control.Concurrent.Async                (AsyncCancelled (..))
import           Control.Concurrent.STM
import           Control.Exception                       (catch, onException)
import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Development.IDE.Graph                   (shakeOptions)
import           Development.IDE.Graph.Database          (shakeNewDatabase,
                                                          shakeRunDatabase,
                                                          shakeRunDatabaseForKeys)
import           Development.IDE.Graph.Internal.Database (build, incDatabase)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Rule
import           Example
import qualified StmContainers.Map                       as STM
import           System.Timeout                          (timeout)
import           Test.Hspec



spec :: Spec
spec = do
  describe "apply1" $ it "Test build update, Buggy dirty mechanism in hls-graph #4237" $ do
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
    db <- shakeNewDatabase shakeOptions $ do
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
  describe "apply1" $ do
    it "computes a rule with no dependencies" $ do
      db <- shakeNewDatabase shakeOptions ruleUnit
      res <- shakeRunDatabase db $
        pure $ apply1 (Rule @())
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
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
      resultDeps res `shouldBe` ResultDeps [singletonKeySet $ newKey (Rule @())]
    it "tracks reverse dependencies" $ do
      db@(ShakeDatabase _ _ Database {..}) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabase db $
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just KeyDetails {..} <- atomically $ STM.lookup (newKey (Rule @())) databaseValues
      keyReverseDeps `shouldBe` singletonKeySet (newKey theKey)
    it "rethrows exceptions" $ do
      db <- shakeNewDatabase shakeOptions $ addRule $ \(Rule :: Rule ()) _old _mode -> error "boom"
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

  describe "applyWithoutDependency" $ it "does not track dependencies" $ do
    db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
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

  describe "Async registry" $ do
    it "registers a running computation and drops it on normal completion" $ do
      started <- C.newEmptyMVar
      proceed <- C.newEmptyMVar :: IO (MVar ())
      done    <- C.newEmptyMVar
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $
        addRule $ \(Rule :: Rule ()) _old _mode -> do
          liftIO $ C.putMVar started () >> C.takeMVar proceed
          return $ RunResult ChangedRecomputeDiff "" () (return ())
      forkSwallowCancel (shakeRunDatabase db [apply1 (Rule @())] >> C.putMVar done ())
      C.takeMVar started
      runningCount theDb `shouldReturn` 1
      C.putMVar proceed ()
      _ <- C.takeMVar done
      runningCount theDb `shouldReturn` 0
    it "cancels an in-flight computation on restart so no thread leaks" $ do
      started   <- C.newEmptyMVar
      proceed   <- C.newEmptyMVar :: IO (MVar ())  -- never filled, so the rule blocks here
      cancelled <- C.newEmptyMVar
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $
        addRule $ \(Rule :: Rule ()) _old _mode -> do
          liftIO $ (C.putMVar started () >> C.takeMVar proceed)
                     `onException` C.putMVar cancelled ()
          return $ RunResult ChangedRecomputeDiff "" () (return ())
      forkSwallowCancel (shakeRunDatabase db [apply1 (Rule @())])
      C.takeMVar started
      runningCount theDb `shouldReturn` 1
      incDatabase theDb (Just [])
      -- The rule's thread received the async exception
      got <- timeout 5_000_000 (C.takeMVar cancelled) -- 5s
      got `shouldBe` Just ()
      runningCount theDb `shouldReturn` 0
    it "drains nested in-flight computations on restart" $ do
      started <- C.newEmptyMVar
      proceed <- C.newEmptyMVar :: IO (MVar ())
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        addRule $ \(Rule :: Rule ()) _old _mode -> do
          liftIO $ C.putMVar started () >> C.takeMVar proceed
          return $ RunResult ChangedRecomputeDiff "" () (return ())
        addRule $ \(Rule :: Rule Bool) _old _mode -> do
          () <- apply1 (Rule @())
          return $ RunResult ChangedRecomputeDiff "" True (return ())
      forkSwallowCancel (shakeRunDatabase db [apply1 (Rule @Bool)])
      C.takeMVar started
      -- Parent `Rule Bool` blocks on the leaf `Rule ()`.
      n <- runningCount theDb
      n `shouldSatisfy` (>= 2)
      -- The drain loop reaps the whole chain, not just the roots.
      incDatabase theDb (Just [])
      runningCount theDb `shouldReturn` 0
  where
    forkSwallowCancel act = void . C.forkIO $ void act `catch` \AsyncCancelled -> pure ()
    runningCount :: Database -> IO Int
    runningCount theDb = length . snd <$> readTVarIO (databaseAsyncs theDb)
