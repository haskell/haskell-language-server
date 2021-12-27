{-# LANGUAGE MultiWayIf #-}
module HieDbRetry (tests) where

import           Control.Concurrent.Extra     (Var, modifyVar, newVar, readVar,
                                               withVar)
import           Control.Exception            (ErrorCall (ErrorCall), evaluate,
                                               throwIO, tryJust)
import           Data.Text                    (Text)
import           Data.Tuple.Extra             (dupe)
import qualified Database.SQLite.Simple       as SQLite
import           Development.IDE.Session      (retryOnException,
                                               retryOnSqliteBusy)
import           Development.IDE.Types.Logger (Logger (Logger), Priority,
                                               noLogging)
import qualified System.Random                as Random
import           Test.Tasty                   (TestTree, testGroup)
import           Test.Tasty.HUnit             (assertFailure, testCase, (@?=))

makeLogger :: Var [(Priority, Text)] -> Logger
makeLogger msgsVar = Logger $ \priority msg -> modifyVar msgsVar (\msgs -> pure ((priority, msg) : msgs, ()))

rng :: Random.StdGen
rng = Random.mkStdGen 0

retryOnSqliteBusyForTest :: Logger -> Int -> IO a -> IO a
retryOnSqliteBusyForTest logger maxRetryCount = retryOnException isErrorBusy logger 1 1 maxRetryCount rng

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
  [ testCase "retryOnException throws exception after max retries" $ do
      logMsgsVar <- newVar []
      let logger = makeLogger logMsgsVar
      let maxRetryCount = 1

      result <- tryJust isErrorBusy (retryOnSqliteBusyForTest logger maxRetryCount (throwIO errorBusy))

      case result of
        Left exception -> do
          exception @?= errorBusy
          withVar logMsgsVar $ \logMsgs ->
            length logMsgs @?= 2
            -- uncomment if want to compare log msgs
            -- logMsgs @?= []
        Right _ -> assertFailure "Expected ErrorBusy exception"

   , testCase "retryOnException doesn't throw if given function doesn't throw" $ do
      let expected = 1 :: Int
      let maxRetryCount = 0

      actual <- retryOnSqliteBusyForTest noLogging maxRetryCount (pure expected)

      actual @?= expected

   , testCase "retryOnException retries the number of times it should" $ do
      countVar <- newVar 0
      let maxRetryCount = 3
      let incrementThenThrow = modifyVar countVar (\count -> pure (dupe (count + 1))) >> throwIO errorBusy

      _ <- tryJust isErrorBusy (retryOnSqliteBusyForTest noLogging maxRetryCount incrementThenThrow)

      withVar countVar $ \count ->
        count @?= maxRetryCount + 1

   , testCase "retryOnException doesn't retry if exception is not ErrorBusy" $ do
      countVar <- newVar (0 :: Int)
      let maxRetryCount = 1

      let throwThenIncrement = do
            count <- readVar countVar
            if count == 0 then
              evaluate (error "dummy exception")
            else
              modifyVar countVar (\count -> pure (dupe (count + 1)))


      _ <- tryJust isErrorCall (retryOnSqliteBusyForTest noLogging maxRetryCount throwThenIncrement)

      withVar countVar $ \count ->
        count @?= 0

   , testCase "retryOnSqliteBusy retries on ErrorBusy" $ do
      countVar <- newVar (0 :: Int)

      let incrementThenThrowThenIncrement = do
            count <- readVar countVar
            if count == 0 then
              modifyVar countVar (\count -> pure (dupe (count + 1))) >> throwIO errorBusy
            else
              modifyVar countVar (\count -> pure (dupe (count + 1)))

      _ <- retryOnSqliteBusy noLogging rng incrementThenThrowThenIncrement

      withVar countVar $ \count ->
        count @?= 2

    , testCase "retryOnException exponentially backs off" $ do
       logMsgsVar <- newVar ([] :: [(Priority, Text)])

       let maxDelay = 100
       let baseDelay = 1
       let maxRetryCount = 6
       let logger = makeLogger logMsgsVar

       result <- tryJust isErrorBusy (retryOnException isErrorBusy logger maxDelay baseDelay maxRetryCount rng (throwIO errorBusy))

       case result of
         Left _ -> do
           withVar logMsgsVar $ \logMsgs ->
             if | ((_, lastLogMsg) : _) <- logMsgs ->
                  -- uses log messages to indirectly check backoff...
                  lastLogMsg @?= "Retries exhausted - base delay: 64, maximumDelay: 100, maxRetryCount: 0, exception: SQLite3 returned ErrorBusy while attempting to perform : "
                | otherwise -> assertFailure "Expected more than 0 log messages"
         Right _ -> assertFailure "Expected ErrorBusy exception"
  ]
