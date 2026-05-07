{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ActionSpec where

import           Control.Concurrent                      (MVar, readMVar)
import qualified Control.Concurrent                      as C
import           Control.Concurrent.STM
import           Control.Exception                       (SomeException)
import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Data.Typeable                           (Typeable)
import           Development.IDE.Graph                   (RuleResult,
                                                          shakeOptions)
import           Development.IDE.Graph.Classes           (Hashable)
import           Development.IDE.Graph.Database          (RuntimeRestartKeys (..),
                                                          mkDelayedAction,
                                                          shakeComputeToPreserve,
                                                          shakeNewDatabase,
                                                          shakeRunDatabase,
                                                          shakeRunDatabaseForKeys,
                                                          shakeShutDatabase)
import           Development.IDE.Graph.Internal.Action   (actionCatch,
                                                          actionFinally,
                                                          pumpActionThreadReRun)
import           Development.IDE.Graph.Internal.Database (build, incDatabase)
import           Development.IDE.Graph.Internal.Key
import           Development.IDE.Graph.Internal.Types
import           Development.IDE.Graph.Rule
import           Example
import qualified StmContainers.Map                       as STM
import           System.Timeout                          (timeout)
import           Test.Hspec



buildWithRoot :: forall f key value . (Traversable f, RuleResult key ~ value, Typeable key, Show key, Hashable key, Typeable value) => Database -> Stack -> f key -> IO (f Key, f value)
buildWithRoot = build (newKey ("root" :: [Char]))

itInThread :: String -> IO () -> SpecWith ()
itInThread = it

shakeRunDatabaseFromRight :: ShakeDatabase -> [Action a] -> IO [a]
shakeRunDatabaseFromRight = shakeRunDatabase

waitForRuntimeRootDep :: Database -> Key -> Key -> IO ()
waitForRuntimeRootDep Database{..} child parent =
  atomically $ do
    deps <- STM.lookup child databaseRRuntimeDepRoot
    check $ maybe False (memberKeySet parent) deps

spec :: Spec
spec = do
  describe "apply1" $ itInThread "Test build update, Buggy dirty mechanism in hls-graph #4237" $ do
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
    _ <- shakeRunDatabaseFromRight db $ pure $ apply1 CountRule -- count = 1
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
    itInThread "computes a rule with no dependencies" $ do
      db <- shakeNewDatabase shakeOptions ruleUnit
      res <- shakeRunDatabaseFromRight db $
        pure $ apply1 (Rule @())
      res `shouldBe` [()]
    itInThread "computes a rule with one dependency" $ do
      db <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      res <- shakeRunDatabaseFromRight db $ pure $ apply1 Rule
      res `shouldBe` [True]
    itInThread "tracks direct dependencies" $ do
      db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabaseFromRight db $
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
      resultDeps res `shouldBe` ResultDeps [singletonKeySet $ newKey (Rule @())]
    itInThread "tracks reverse dependencies" $ do
      db@(ShakeDatabase _ _ Database {..}) <- shakeNewDatabase shakeOptions $ do
        ruleUnit
        ruleBool
      let theKey = Rule @Bool
      res <- shakeRunDatabaseFromRight db $
        pure $ apply1 theKey
      res `shouldBe` [True]
      Just KeyDetails {..} <- atomically $ STM.lookup (newKey (Rule @())) databaseValues
      keyReverseDeps `shouldBe` singletonKeySet (newKey theKey)
    itInThread "rethrows exceptions" $ do
      db <- shakeNewDatabase shakeOptions $ addRule $ \(Rule :: Rule ()) _old _mode -> error "boom"
      let res = shakeRunDatabaseFromRight db $ pure $ apply1 (Rule @())
      res `shouldThrow` anyErrorCall
    itInThread "restart kills a delayed action parked behind a caught producer failure" $ do
      producerStarted <- C.newEmptyMVar
      releaseProducer <- C.newEmptyMVar
      producerCaught <- C.newEmptyMVar
      waiterFinalized <- C.newEmptyMVar
      sdb@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $
        addRule $ \(Rule :: Rule Int) _old _mode -> do
          liftIO $ void $ C.tryPutMVar producerStarted ()
          liftIO $ readMVar releaseProducer
          error "boom"
      producer <- mkDelayedAction "producer" Debug $
        actionCatch @SomeException
          (void $ apply1 (Rule @Int))
          (\_ -> liftIO $ void $ C.tryPutMVar producerCaught ())
      waiter <- mkDelayedAction "waiter" Debug $
        actionFinally
          (do
            liftIO $ readMVar producerStarted
            void $ apply1 (Rule @Int))
          (void $ C.tryPutMVar waiterFinalized ())

      _ <- shakeRunDatabaseForKeys Nothing sdb
        [ pumpActionThreadReRun sdb producer
        , pumpActionThreadReRun sdb waiter
        ]
      let dirtyKey = newKey (Rule @Int)
      waitForRuntimeRootDep theDb dirtyKey (uniqueID waiter)
      C.putMVar releaseProducer ()
      readMVar producerCaught
      C.tryReadMVar waiterFinalized >>= (`shouldBe` Nothing)

      runtimeRestartKeys <- shakeComputeToPreserve sdb (singletonKeySet dirtyKey)
      uniqueID waiter `memberKeySet` restartKillKeys runtimeRestartKeys `shouldBe` True
      shakeShutDatabase (restartKillKeys runtimeRestartKeys) sdb
      timeout 1000000 (readMVar waiterFinalized) >>= (`shouldBe` Just ())
    itInThread "computes a rule with branching dependencies does not invoke phantom dependencies #3423" $ do
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
      res0 <- buildWithRoot theDb emptyStack [BranchedRule]
      snd res0 `shouldBe` [1 :: Int]
      _ <- incDatabase theDb Nothing
      -- build the one with the condition False
      -- This should not call the SubBranchRule
      res1 <- buildWithRoot theDb emptyStack [BranchedRule]
      snd res1 `shouldBe` [2 :: Int]
      -- SubBranchRule should be recomputed once before this (when the condition was True)
      countRes <- buildWithRoot theDb emptyStack [SubBranchRule]
      snd countRes `shouldBe` [1 :: Int]

  describe "applyWithoutDependency" $ itInThread "does not track dependencies" $ do
    db@(ShakeDatabase _ _ theDb) <- shakeNewDatabase shakeOptions $ do
      ruleUnit
      addRule $ \Rule _old _mode -> do
          [()] <- applyWithoutDependency [Rule]
          return $ RunResult ChangedRecomputeDiff "" True $ return ()

    let theKey = Rule @Bool
    res <- shakeRunDatabaseFromRight db $
      pure $ applyWithoutDependency [theKey]
    res `shouldBe` [[True]]
    Just (Clean res) <- lookup (newKey theKey) <$> getDatabaseValues theDb
    resultDeps res `shouldBe` UnknownDeps
