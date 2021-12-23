{-# LANGUAGE MultiWayIf #-}
module HieDbRetry (tests) where

import           Control.Concurrent.Extra     (Var, modifyVar, newVar, readVar,
                                               withVar)
import           Control.Exception            (ErrorCall (ErrorCall), evaluate,
                                               throwIO, tryJust)
import           Data.Text                    (Text)
import           Data.Tuple.Extra             (dupe)
import qualified Database.SQLite.Simple       as SQLite
import           Development.IDE.Core.Shake   (HieDb)
import           Development.IDE.Session      (retryOnSqliteBusy)
import           Development.IDE.Types.Logger (Logger (Logger), Priority,
                                               noLogging)
import qualified System.Random                as Random
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (assertFailure, testCase, (@?=))

makeLogger :: Var [(Priority, Text)] -> Logger
makeLogger msgsVar = Logger $ \priority msg -> modifyVar msgsVar (\msgs -> pure ((priority, msg) : msgs, ()))

rng :: Random.StdGen
rng = Random.mkStdGen 0

defaultRetryOnSqliteBusy :: Logger -> Int -> (HieDb -> IO b) -> IO b
defaultRetryOnSqliteBusy logger maxRetryCount = retryOnSqliteBusy logger undefined 1 1 maxRetryCount rng

isErrorBusy :: SQLite.SQLError -> Maybe SQLite.SQLError
isErrorBusy e
  | SQLite.SQLError { sqlError = SQLite.ErrorBusy } <- e = Just e
  | otherwise = Nothing

errorBusy :: SQLite.SQLError
errorBusy = SQLite.SQLError{ sqlError = SQLite.ErrorBusy, sqlErrorDetails = "", sqlErrorContext = "" }

isErrorCall :: ErrorCall -> Maybe ErrorCall
isErrorCall e
  | ErrorCall _ <- e = Just e
  | otherwise = Nothing

tests :: TestTree
tests = testGroup "RetryHieDb"
  [ testCase "retryOnSqliteBusy throws ErrorBusy after max retries" $ do
      logMsgsVar <- newVar []
      let logger = makeLogger logMsgsVar
      let maxRetryCount = 1
      let action = defaultRetryOnSqliteBusy logger maxRetryCount (\_ -> throwIO errorBusy)

      result <- tryJust isErrorBusy action

      case result of
        Left exception -> do
          exception @?= errorBusy
          withVar logMsgsVar $ \logMsgs ->
            length logMsgs @?= 2
            -- uncomment if want to compare log msgs
            -- logMsgs @?= []
        Right _ -> assertFailure "Expected ErrorBusy exception"

   , testCase "retryOnSqliteBusy doesn't throw if given function doesn't throw" $ do
      let expected = 1 :: Int
      let maxRetryCount = 0
      let action = defaultRetryOnSqliteBusy noLogging maxRetryCount (\_ -> pure expected)

      actual <- action

      actual @?= expected

   , testCase "retryOnSqliteBusy retries the number of times it should" $ do
      countVar <- newVar 0
      let maxRetryCount = 3
      let hieDbAction _ = modifyVar countVar (\count -> pure (dupe (count + 1))) >> throwIO errorBusy
      let action = defaultRetryOnSqliteBusy noLogging maxRetryCount hieDbAction

      _ <- tryJust isErrorBusy action

      withVar countVar $ \count ->
        count @?= maxRetryCount + 1

   , testCase "retryOnSqliteBusy doesn't retry if exception is not ErrorBusy" $ do
      countVar <- newVar (0 :: Int)
      let maxRetryCount = 1

      let hieDbAction _ = do
            count <- readVar countVar
            if count == 0 then
              evaluate (error "dummy exception")
            else
              modifyVar countVar (\count -> pure (dupe (count + 1)))

      let action = defaultRetryOnSqliteBusy noLogging maxRetryCount hieDbAction

      _ <- tryJust isErrorCall action

      withVar countVar $ \count ->
        count @?= 0

    , testCase "retryOnSqliteBusy exponentially backs off" $ do
       logMsgsVar <- newVar ([] :: [(Priority, Text)])

       let maxDelay = 100
       let baseDelay = 1
       let maxRetryCount = 6
       let logger = makeLogger logMsgsVar
       let action = retryOnSqliteBusy logger undefined maxDelay baseDelay maxRetryCount rng (\_ -> throwIO errorBusy)

       result <- tryJust isErrorBusy action

       case result of
         Left _ -> do
           withVar logMsgsVar $ \logMsgs ->
             if | ((_, lastLogMsg) : _) <- logMsgs ->
                  -- uses log messages to indirectly check backoff...
                  lastLogMsg @?= "Retries exhausted - base delay: 64, maximumDelay: 100, maxRetryCount: 0, exception: SQLite3 returned ErrorBusy while attempting to perform : "
                | otherwise -> assertFailure "Expected more than 0 log messages"
         Right _ -> assertFailure "Expected ErrorBusy exception"
  ]
