{-# LANGUAGE MultiWayIf #-}
module HieDbRetry (tests) where

import           Control.Concurrent.Extra (Var, modifyVar, newVar, readVar,
                                           withVar)
import           Control.Exception        (ErrorCall (ErrorCall), evaluate,
                                           throwIO, tryJust)
import           Control.Monad.IO.Class   (MonadIO (liftIO))
import           Data.Tuple.Extra         (dupe)
import qualified Database.SQLite.Simple   as SQLite
import           Development.IDE.Session  (retryOnException, retryOnSqliteBusy)
import qualified Development.IDE.Session  as Session
import           Ide.Logger               (Recorder (Recorder, logger_),
                                           WithPriority (WithPriority, payload),
                                           cmapWithPrio)
import qualified System.Random            as Random
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit         (assertFailure, testCase, (@?=))

data Log
  = LogSession Session.Log
  deriving Show

makeLogger :: Var [Log] -> Recorder (WithPriority Log)
makeLogger msgsVar =
  Recorder {
    logger_ = \WithPriority{ payload = msg } -> liftIO $ modifyVar msgsVar (\msgs -> pure (msg : msgs, ()))
  }

rng :: Random.StdGen
rng = Random.mkStdGen 0

retryOnSqliteBusyForTest :: Recorder (WithPriority Log) -> Int -> IO a -> IO a
retryOnSqliteBusyForTest recorder maxRetryCount = retryOnException isErrorBusy (cmapWithPrio LogSession recorder) 1 1 maxRetryCount rng

isErrorBusy :: SQLite.SQLError -> Maybe SQLite.SQLError
isErrorBusy e
  | SQLite.SQLError { sqlError = SQLite.ErrorBusy } <- e = Just e
  | otherwise = Nothing

errorBusy :: SQLite.SQLError
errorBusy = SQLite.SQLError{ sqlError = SQLite.ErrorBusy, sqlErrorDetails = "", sqlErrorContext = "" }

isErrorCall :: ErrorCall -> Maybe ErrorCall
isErrorCall e
  | ErrorCall _ <- e = Just e

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

      actual <- retryOnSqliteBusyForTest mempty maxRetryCount (pure expected)

      actual @?= expected

   , testCase "retryOnException retries the number of times it should" $ do
      countVar <- newVar 0
      let maxRetryCount = 3
      let incrementThenThrow = modifyVar countVar (\count -> pure (dupe (count + 1))) >> throwIO errorBusy

      _ <- tryJust isErrorBusy (retryOnSqliteBusyForTest mempty maxRetryCount incrementThenThrow)

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


      _ <- tryJust isErrorCall (retryOnSqliteBusyForTest mempty maxRetryCount throwThenIncrement)

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

      _ <- retryOnSqliteBusy mempty rng incrementThenThrowThenIncrement

      withVar countVar $ \count ->
        count @?= 2

    , testCase "retryOnException exponentially backs off" $ do
       logMsgsVar <- newVar ([] :: [Log])

       let maxDelay = 100
       let baseDelay = 1
       let maxRetryCount = 6
       let logger = makeLogger logMsgsVar

       result <- tryJust isErrorBusy (retryOnException isErrorBusy (cmapWithPrio LogSession logger) maxDelay baseDelay maxRetryCount rng (throwIO errorBusy))

       case result of
         Left _ -> do
           withVar logMsgsVar $ \logMsgs ->
             -- uses log messages to check backoff...
             if | (LogSession (Session.LogHieDbRetriesExhausted baseDelay maximumDelay maxRetryCount _) : _) <- logMsgs -> do
                  baseDelay @?= 64
                  maximumDelay @?= 100
                  maxRetryCount @?= 0
                | otherwise -> assertFailure "Expected more than 0 log messages"
         Right _ -> assertFailure "Expected ErrorBusy exception"
  ]
