{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ActionSpec where

import           Control.Concurrent                      (MVar, readMVar)
import qualified Control.Concurrent                      as C
import           Control.Concurrent.STM
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

  describe "Discard superseded computations" $ do
    it "leaves a key dirty when a restart bumps the step mid-computation" $ do
      started <- C.newEmptyMVar
      proceed <- C.newEmptyMVar
      done    <- C.newEmptyMVar
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $
        addRule $ \(Rule :: Rule ()) _old _mode -> do
          liftIO $ C.putMVar started ()
          liftIO $ C.takeMVar proceed
          return $ RunResult ChangedRecomputeDiff "" () (return ())
      -- Fork so a restart can bump the step while the rule is still computing.
      _ <- C.forkIO $ shakeRunDatabase db [apply1 (Rule @())] >>= C.putMVar done
      C.takeMVar started
      -- Bumps the step without dirtying anything, so only the guard can leave
      -- this key dirty.
      incDatabase theDb (Just [])
      C.putMVar proceed ()
      _ <- C.takeMVar done
      Just status <- lookup (newKey (Rule @())) <$> getDatabaseValues theDb
      case status of
        Dirty{}   -> pure ()
        Clean{}   -> expectationFailure "superseded computation was committed clean"
        Running{} -> expectationFailure "superseded computation left running"
    it "commits clean when the step does not advance" $ do
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $
        addRule $ \(Rule :: Rule ()) _old _mode ->
          return $ RunResult ChangedRecomputeDiff "" () (return ())
      _ <- shakeRunDatabase db [apply1 (Rule @())]
      Just status <- lookup (newKey (Rule @())) <$> getDatabaseValues theDb
      case status of
        Clean{} -> pure ()
        _       -> expectationFailure "expected a clean commit"
    it "leaves a newer build's Running intact instead of stomping it" $
      withSupersededRespawn $ \theDb proceedA doneA proceedB doneB -> do
        -- A finishes while B is still Running{2}. Guard must keep B's Running.
        C.putMVar proceedA ()
        _ <- C.takeMVar doneA
        status <- lookup (newKey (Rule @())) <$> getDatabaseValues theDb
        -- Release B before asserting so its thread finishes.
        C.putMVar proceedB ()
        _ <- C.takeMVar doneB
        case status of
          Just Running{} -> pure ()
          Just Dirty{}   -> expectationFailure "superseded build unnecessary marked dirty"
          Just Clean{}   -> expectationFailure "newer build committed too early"
          Nothing        -> expectationFailure "key missing from the database"
    it "leaves a newer build's committed Clean intact instead of dirtying it" $
      withSupersededRespawn $ \theDb proceedA doneA proceedB doneB -> do
        -- B commits Clean{2} before A demotes. Guard must keep B's Clean.
        C.putMVar proceedB ()
        _ <- C.takeMVar doneB
        C.putMVar proceedA ()
        _ <- C.takeMVar doneA
        status <- lookup (newKey (Rule @())) <$> getDatabaseValues theDb
        case status of
          Just Clean{}   -> pure ()
          Just Dirty{}   -> expectationFailure "superseded build unnecessary marked dirty"
          Just Running{} -> expectationFailure "newer build didn't commit"
          Nothing        -> expectationFailure "key missing from the database"
  where
    -- Two builds of the same key.
    --
    -- 1. A, the superseded build, runs at step 1.
    -- 2. B, the re-spawn, runs at step 2. B's shakeRunDatabase bumps the step
    --    and re-dirties A's in-flight key, so the rule runs again and leaves B
    --    Running{2}.
    --
    -- Both are started and blocked before the continuation runs. The
    -- continuation picks the release ordering that decides whether the guard
    -- meets B as Running or as Clean.
    withSupersededRespawn
      :: (Database -> MVar () -> MVar () -> MVar () -> MVar () -> IO ())
      -> IO ()
    withSupersededRespawn k = do
      calls    <- newTVarIO (0 :: Int)
      startedA <- C.newEmptyMVar
      proceedA <- C.newEmptyMVar
      startedB <- C.newEmptyMVar
      proceedB <- C.newEmptyMVar
      doneA    <- C.newEmptyMVar
      doneB    <- C.newEmptyMVar
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $
        addRule $ \(Rule :: Rule ()) _old _mode -> do
          n <- liftIO $ atomically $ modifyTVar' calls (+1) >> readTVar calls
          liftIO $ if n == 1
            then C.putMVar startedA () >> C.takeMVar proceedA
            else C.putMVar startedB () >> C.takeMVar proceedB
          return $ RunResult ChangedRecomputeDiff "" () (return ())
      _ <- C.forkIO $ shakeRunDatabase db [apply1 (Rule @())] >> C.putMVar doneA ()
      C.takeMVar startedA
      _ <- C.forkIO $ shakeRunDatabase db [apply1 (Rule @())] >> C.putMVar doneB ()
      C.takeMVar startedB
      k theDb proceedA doneA proceedB doneB
